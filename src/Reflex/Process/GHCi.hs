{-|
 - Module: Reflex.Process.GHCi
 - Description: Run GHCi processes in a reflex application
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Reflex.Process.GHCi
  ( ghci
  , ghciWatch
  , Ghci(..)
  , Status(..)
  , moduleOutput
  , execOutput
  , collectOutput
  , statusMessage
  ) where

import Reflex
import Reflex.FSNotify (watchDirectory)
import Reflex.Process (ProcessConfig(..), Process(..), createProcess)

import Control.Monad ((<=<))
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.String (IsString)
import System.Directory (getCurrentDirectory)
import qualified System.FSNotify as FS
import System.FilePath.Posix (takeExtension)
import qualified System.Process as P
import qualified Text.Regex.TDFA as Regex ((=~))

-- | Runs a GHCi process and reloads it whenever the provided event fires
ghci
  :: ( TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadIO m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -- ^ Command to run to enter GHCi
  -> Maybe ByteString
  -- ^ Expression to evaluate whenever GHCi successfully loads modules
  -> Event t ()
  -- ^ Ask GHCi to reload
  -> m (Ghci t)
ghci p mexpr reloadReq = do

  let cmd = p { P.create_group = True } -- Need to set this so that group interrupts don't impact parent

  -- Run the process and feed it some input:
  rec proc <- createProcess cmd $ ProcessConfig
        { _processConfig_stdin = leftmost
            [ reload
            -- Execute some expression if GHCi is ready to receive it
            , fforMaybe (updated status) $ \case
                Status_LoadSucceeded -> mexpr
                _ -> Nothing
            -- On first load, set the prompt
            , let f old new = if old == Status_Initializing && new == Status_Loading
                    then Just $ C8.intercalate "\n"
                      [ "Prelude.putStrLn \"Initialized. Setting up reflex-ghci...\""
                      , ":set prompt ..."
                      , ":set -fno-break-on-exception"
                      , ":set -fno-break-on-error"
                      , ":set prompt \"\""
                      , "Prelude.putStrLn \"\""
                      , ":set prompt " <> prompt
                      , ":r"
                      ]
                    else Nothing
              in attachWithMaybe f (current status) (updated status)
            ]
        , _processConfig_signal = never
        }

      -- Reload
      let reload = leftmost
            [ ":r" <$ reloadReq
            ]

      -- Capture and accumulate stdout and stderr between reloads.
      -- We'll inspect these values to determine GHCi's state
      output <- collectOutput (() <$ reload) $ _process_stdout proc
      errors <- collectOutput (() <$ reload) $ _process_stderr proc

     -- Only interrupt when there's a file change and we're ready and not in an idle state
      let interruptible s = s `elem` [Status_Loading, Status_Executing]
          requestInterrupt = gate (interruptible <$> current status) $ (() <$ reloadReq)
      performEvent_ $ ffor requestInterrupt $
        const $ liftIO $ P.interruptProcessGroupOf $ _process_handle proc

      -- Define some Regex patterns to use to determine GHCi's state based on output
      let okModulesLoaded = "Ok.*module.*loaded." :: ByteString
          failedNoModulesLoaded = "Failed,.*modules loaded." :: ByteString
          -- TODO: Is there a way to distinguish GHCi's actual exception output
          -- from someone printing "*** Exception:" to stderr?
          -- TODO: Are there other exception patterns to watch out for?
          exceptionMessage = "\\*\\*\\* Exception:.*" :: ByteString
          interactiveErrorMessage = "<interactive>:.*:.*:.error:.*" :: ByteString
          -- We need to know when ghci is initialized enough that it won't die when
          -- it receives an interrupt. We wait to see the version line in the output as
          -- a proxy for GHCi's readiness to be interrupted
          ghciVersionMessage = "GHCi, version.*: http://www.haskell.org/ghc/" :: ByteString


      -- Inspect the output and determine what state GHCi is in
      status :: Dynamic t Status <- holdUniqDyn <=< foldDyn ($) Status_Initializing $ leftmost
        [ fforMaybe (updated errors) $ \err -> if err Regex.=~ exceptionMessage || err Regex.=~ interactiveErrorMessage
          then Just $ const Status_ExecutionFailed
          else Nothing
        , const Status_Loading <$ reload
        , ffor (updated output) $ \out -> case reverse (C8.lines out) of
            (lastLine:expectedMessage:_)
              | lastLine == prompt && expectedMessage Regex.=~ okModulesLoaded -> const Status_LoadSucceeded
              | lastLine == prompt && expectedMessage Regex.=~ failedNoModulesLoaded -> const Status_LoadFailed
              | lastLine == prompt -> \case
                  Status_Executing -> Status_ExecutionSucceeded
                  s -> s
              | lastLine Regex.=~ ghciVersionMessage -> const Status_Loading
              | otherwise -> \case
                  Status_LoadSucceeded -> case mexpr of
                    Nothing -> Status_LoadSucceeded
                    Just _ -> Status_Executing
                  s -> s
            _ -> id
        ]

  -- Determine when to switch output stream from GHCi module output to execution output
  execStream <- hold False $ leftmost
      [ False <$ reload
      , fforMaybe (updated status) $ \case
          Status_LoadSucceeded -> Just True
          Status_LoadFailed -> Just False
          Status_Executing -> Just True
          _ -> Nothing
      ]

  -- Below, we split up the output of the GHCi process into things that GHCi itself
  -- produces (e.g., errors, warnings, loading messages) and the output of the expression
  -- it is evaluating
  return $ Ghci
    { _ghci_moduleOut = gate (not <$> execStream) $ _process_stdout proc
    , _ghci_moduleErr = gate (not <$> execStream) $ _process_stderr proc
    , _ghci_execOut = gate execStream $ _process_stdout proc
    , _ghci_execErr = gate execStream $ _process_stderr proc
    , _ghci_reload = () <$ reload
    , _ghci_status = status
    , _ghci_process = proc
    }
  where
    prompt :: IsString a => a
    prompt = "<| Waiting |>"

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
     )
  => P.CreateProcess
  -> Maybe ByteString
  -> m (Ghci t)
ghciWatch p mexec = do
  -- Get the current directory so we can observe changes in it
  dir <- liftIO getCurrentDirectory

  -- TODO: Separate the filesystem event logic into its own function
  -- Watch the project directory for changes
  pb <- getPostBuild
  fsEvents <- watchDirectory (noDebounce FS.defaultConfig) (dir <$ pb)

  -- TODO Handle changes to "src" and ".cabal" differently. ":r" is only really appropriate
  -- when there are changes to loaded modules.
  -- We could use ":show modules" to see which hs files are loaded and determine what to do based
  -- on that, but we'll need to parse that output.
  let filteredFsEvents = flip ffilter fsEvents $ \e -> 
        takeExtension (FS.eventPath e) `elem` [".hs", ".lhs"]

  -- Events are batched because otherwise we'd get several updates corresponding to one
  -- user-level change. For example, saving a file in vim results in an event claiming
  -- the file was removed followed almost immediately by an event adding the file
  batchedFsEvents <- batchOccurrences 0.1 filteredFsEvents

  -- Call GHCi and request a reload every time the files we're watching change
  ghci p mexec $ () <$ batchedFsEvents
  where
    noDebounce :: FS.WatchConfig -> FS.WatchConfig
    noDebounce cfg = cfg { FS.confDebounce = FS.NoDebounce }

-- | The output of the GHCi process
data Ghci t = Ghci
  { _ghci_moduleOut :: Event t ByteString
  -- ^ stdout output produced when loading modules
  , _ghci_moduleErr :: Event t ByteString
  -- ^ stderr output produced when loading modules
  , _ghci_execOut :: Event t ByteString
  -- ^ stdout output produced while evaluating an expression
  , _ghci_execErr :: Event t ByteString
  -- ^ stderr output produced while evaluating an expression
  , _ghci_reload :: Event t ()
  -- ^ Event that fires when GHCi is reloading
  , _ghci_status :: Dynamic t Status
  -- ^ The current status of the GHCi process
  , _ghci_process :: Process t
  }

-- | The state of the GHCi process
data Status
  = Status_Initializing
  | Status_Loading
  | Status_LoadFailed
  | Status_LoadSucceeded
  | Status_Executing
  | Status_ExecutionFailed
  | Status_ExecutionSucceeded
  deriving (Show, Read, Eq, Ord)

-- | Collect all the GHCi module output (i.e., errors, warnings, etc) and optionally clear
-- every time GHCi reloads
moduleOutput
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Behavior t Bool
  -- ^ Whether to clear the output on reload
  -> Ghci t
  -> m (Dynamic t ByteString)
moduleOutput clear g = collectOutput
  (gate clear $ () <$ _ghci_reload g) $
    leftmost [_ghci_moduleOut g, _ghci_moduleErr g]

-- | Collect all the GHCi expression output (i.e., the output of the called function) and optionally clear
-- every time GHCi reloads
execOutput
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Behavior t Bool
  -- ^ Whether to clear the output on reload
  -> Ghci t
  -> m (Dynamic t ByteString)
execOutput clear g = collectOutput
  (gate clear $ () <$ _ghci_reload g) $
    leftmost [_ghci_execOut g, _ghci_execErr g]

-- | Collect output, appending new output to the end of the accumulator
collectOutput
  :: (Reflex t, MonadFix m, MonadHold t m)
  => Event t ()
  -- ^ Clear output
  -> Event t ByteString
  -- ^ Output to add
  -> m (Dynamic t ByteString)
collectOutput clear out = foldDyn ($) "" $ leftmost
  [ flip mappend <$> out
  , const "" <$ clear
  ]

-- | Describe the current status of GHCi in a human-readable way
statusMessage :: IsString a => Status -> a
statusMessage = \case
  Status_Initializing -> "Initializing..."
  Status_Loading -> "Loading Modules..."
  Status_LoadFailed -> "Failed to Load Modules!"
  Status_LoadSucceeded -> "Successfully Loaded Modules!"
  Status_Executing -> "Executing Command..."
  Status_ExecutionFailed -> "Command Failed!"
  Status_ExecutionSucceeded -> "Command Succeeded!"
