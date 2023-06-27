{-|
 - Module: Reflex.Process.GHCi
 - Description: Run GHCi processes in a reflex application
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
module Reflex.Process.GHCi
  ( ghci
  , ghciWatch
  , module X
  , hasErrors
  ) where

import Reflex
import Reflex.FSNotify (watchDirectoryTree)
import Reflex.Process (Process(..))

import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map as Map
import System.Directory (getCurrentDirectory)
import qualified System.FSNotify as FS
import System.FilePath.Posix (takeExtension)
import qualified System.Info as Sys
import qualified System.Posix.Signals as Signals
import qualified System.Process as P
import qualified Text.Regex.TDFA as Regex ((=~))

import Reflex.Process.Repl as X

-- | Runs a GHCi process and reloads it whenever the provided event fires
ghci
  :: ( Adjustable t m
     , MonadFix m
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadIO m
     , NotReady t m
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     )
  => P.CreateProcess -- ^ How to run GHCi
  -> Event t [Command] -- ^ Send an expression to evaluate
  -> Event t () -- ^ Reload
  -> Event t () -- ^ Shutdown
  -> m (Repl t)
ghci runGhci expr reload shutdown = do
  _ <- liftIO $ Signals.installHandler Signals.sigINT (Signals.Catch (return ())) Nothing
  pb <- getPostBuild
  rec
    r@(Repl proc _finished started exit) <- repl runGhci inputs isPrompt
    let inputs = mergeWith (<>)
          [ commands setupCommands <$ pb
          , command ":r" <$ interrupted
          , command ":r" <$ reload'
          , command ":q" <$ shutdown
          , expr
          ]
    let interruptible = ffor started $ \case
          (_, Nothing) -> False
          _ -> True
    -- Don't allow reloads too close together. Sometimes a single change that
    -- results in a reload might actually cause multiple reload events (e.g.,
    -- an editor writing a file by deleting and writing it, resulting in two
    -- filesystem events)
    reloadThrottled <- throttle 0.1 reload
    let reload' = gate (not <$> current interruptible) reloadThrottled
    interrupted <- performEvent $ ffor (gate (current interruptible) reloadThrottled) $ \_ -> do
      liftIO $ P.interruptProcessGroupOf $ _process_handle proc
    -- If we can't shut down cleanly within 2 seconds, kill it
    forceShutdown <- delay 2 shutdown
    performEvent_ $ ffor forceShutdown $ \_ -> liftIO $ do
      let h = _process_handle proc
      P.interruptProcessGroupOf h
      P.terminateProcess h
    -- Reinstall the default signal handler after the repl process exits
    performEvent_ $ ffor exit $ \_ -> liftIO $
      void $ Signals.installHandler Signals.sigINT Signals.Default Nothing
  pure r
  where
    promptPostfix :: ByteString
    promptPostfix = "_reflex_ghci_prompt>"
    isPrompt cur line = (C8.pack (show cur) <> promptPostfix) == line
    setupCommands =
      [ ":set prompt-function \\_ x -> let s = \"\\n\" <> show x <> \"" <> promptPostfix <> "\\n\" in System.IO.hPutStr System.IO.stderr s >> pure s"
      , ":set -fno-break-on-exception"
      , ":set -fno-break-on-error"
      , ":set -v1"
      , ":set -fno-hide-source-paths"
      , ":set -ferror-spans"
      , ":set -fdiagnostics-color=never" -- TODO handle ansi escape codes in output
      , ":r" -- This is here because we might hit errors at load time, before we've had a chance to set up the prompt. This will re-print those errors.
      ]

-- | Detect errors reported in stdout or stderr
hasErrors :: Cmd -> Bool
hasErrors (Cmd _ o e) =
  let errs l =
        l Regex.=~ exceptionMessage ||
        l Regex.=~ interactiveErrorMessage ||
        l Regex.=~ moduleLoadError
      errOnStderr = any errs $ _lines_terminated e
      errOnStdout = any (Regex.=~ failedModulesLoaded) $ _lines_terminated o
  in errOnStderr || errOnStdout

-- | Run a GHCi process that watches for changes to Haskell source files in the
-- current directory and reloads if they are modified
ghciWatch
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadIO m
     , MonadFix m
     , MonadHold t m
     , Adjustable t m
     , NotReady t m
     )
  => P.CreateProcess
  -> Maybe Command
  -> Event t ()
  -> Event t ()
  -> m (Repl t)
ghciWatch p mexpr reload shutdown = do
  -- Get the current directory so we can observe changes in it
  dir <- liftIO getCurrentDirectory

  -- TODO: Separate the filesystem event logic into its own function
  -- Watch the project directory for changes
  pb <- getPostBuild
  -- TODO Handle changes to "src" and ".cabal" differently. ":r" is only really appropriate
  -- when there are changes to loaded modules.
  -- We could use ":show modules" to see which hs files are loaded and determine what to do based
  -- on that, but we'll need to parse that output.

  -- On macOS, use the polling backend due to https://github.com/luite/hfsevents/issues/13
  -- TODO check if this is an issue with nixpkgs
  let fsConfig = FS.defaultConfig
        { FS.confWatchMode =
            if Sys.os == "darwin"
              then FS.WatchModePoll 200000
              else FS.WatchModeOS
        }
  fsEvents <- watchDirectoryTree fsConfig (dir <$ pb) $ \e ->
    takeExtension (FS.eventPath e) `elem` [".hs", ".lhs"]

  -- Events are batched because otherwise we'd get several updates corresponding to one
  -- user-level change. For example, saving a file in vim results in an event claiming
  -- the file was removed followed almost immediately by an event adding the file
  batchedFsEvents <- batchOccurrences 0.05 fsEvents

  -- Call GHCi, request a reload every time the files we're watching change.
  let reloadEvents = ((() <$ batchedFsEvents) <> reload)

  rec g <- ghci p sendExpr reloadEvents shutdown
      sendExpr <- delay 0.1 $ fforMaybe (_repl_finished g) $ \finished -> case reverse (Map.elems finished) of
            c@(Cmd cmd _ _):_ -> if displayCommand cmd == ":r" && hasErrors c
              then Nothing
              else case mexpr of
                Nothing -> Nothing
                Just expr -> Just [expr]
            _ -> Nothing
  pure g

failedModulesLoaded :: ByteString
failedModulesLoaded = "Failed,.*module.*loaded." :: ByteString

-- TODO: Is there a way to distinguish GHCi's actual exception output
-- from someone printing "*** Exception:" to stderr?
-- TODO: Are there other exception patterns to watch out for?
exceptionMessage :: ByteString
exceptionMessage = "\\*\\*\\* Exception:.*" :: ByteString

interactiveErrorMessage :: ByteString
interactiveErrorMessage = "<interactive>:.*:.*:.error:.*" :: ByteString

moduleLoadError :: ByteString
moduleLoadError = "^.+\\.(l)?hs:[0-9]+:[0-9]+: error:$"
