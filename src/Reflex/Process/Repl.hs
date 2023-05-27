{-|
 - Module: Reflex.Process.Repl
 - Description: Run repl-like processes in a reflex application.
-}

{-# Language BangPatterns #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language LambdaCase #-}
{-# Language MultiParamTypeClasses #-}
{-# Language MultiWayIf #-}
{-# Language OverloadedStrings #-}
{-# Language RecursiveDo #-}
{-# Language ScopedTypeVariables #-}
{-# Language StandaloneDeriving #-}
{-# Language TupleSections #-}
module Reflex.Process.Repl
  ( Repl(..)
  , Cmd(..)
  , Accum(..)
  , accumHandle
  , accumHandles
  , flushAccum
  , repl
  , Lines(..)
  , emptyLines
  , addLines
  , linesFromBS
  , unLines
  , lastWholeLine
  , splitLinesOn
  , Command
  , unsafeCommand
  , command
  , commands
  , displayCommand
  , sendCommands
  , testRepl
  , mkTestCommands
  , assertStdoutEq
  , assertStderrEq
  , assertStderr
  , assertStdout
  , assertHandleEq
  , assertHandle
  , assertCmd
  ) where

import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Align (align)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C8
import Data.Foldable (toList)
import Data.IORef
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Data.These (These(..))
import Reflex
import Reflex.Host.Headless
import Reflex.Process
import System.Exit (ExitCode)
import qualified System.Posix.Signals as Signals
import qualified System.Process as P

-- * REPL

-- | Output of a 'repl' session
data Repl t = Repl
  { _repl_process :: Process t ByteString ByteString
  -- ^ The underlying repl process, which can be used to access the process handle, the raw output, and so on.
  , _repl_finished :: Event t (Map Int Cmd)
  -- ^ An event of commands that the repl has finished executing and their
  -- associated output. The 'Int' here represents the order in which commands
  -- were submitted to the repl.
  , _repl_started :: Dynamic t (Int, Maybe Cmd)
  -- ^ A dynamic of the currently running command, if any, and its output. The
  -- 'Int' here represents the ordering of this command relative to the ones in
  -- '_repl_finished'.
  , _repl_exited :: Event t ExitCode
  -- ^ An event that fires when the repl exits
  }

-- | An individual command submitted to the repl, and its output
data Cmd = Cmd
  { _cmd_stdin :: Command
  , _cmd_stdout :: Lines
  , _cmd_stderr :: Lines
  }
  deriving (Eq, Ord, Show)

-- | An accumulator used to track and combine the repl's output streams 
data Accum = Accum
  { _accum_stdout :: (Map Int Lines, Int, Lines)
  , _accum_stderr :: (Map Int Lines, Int, Lines)
  }

-- | Accumulate a handle, grouping its output by prompts
accumHandle
  :: (Int -> ByteString -> Bool)
  -> ByteString
  -> (Map Int Lines, Int, Lines)
  -> (Map Int Lines, Int, Lines)
accumHandle isPrompt new (done, cur, l) =
  let l' = addLines new l
  in case splitLinesOn (isPrompt (cur + 1)) l' of
        Nothing -> (done, cur, l')
        Just (before, after) ->
          let (newMap, newCur, newLines) = accumHandle isPrompt (unLines after) (Map.insert cur before done, cur+1, mempty)
          in (Map.union newMap done, newCur, newLines)

-- | Accumulate the output of stdout and stderr, grouping the output lines of both by prompt
accumHandles
  :: (Int -> ByteString -> Bool)
  -> These ByteString ByteString
  -> Accum
  -> Accum
accumHandles isPrompt new acc =
  let acc' = case new of
        This a -> acc
          { _accum_stdout = accumHandle isPrompt a $ _accum_stdout acc
          }
        That a -> acc
          { _accum_stderr = accumHandle isPrompt a $ _accum_stderr acc
          }
        These a b -> acc
          { _accum_stdout = accumHandle isPrompt a $ _accum_stdout acc
          , _accum_stderr = accumHandle isPrompt b $ _accum_stderr acc
          }
      -- This intersection represents the commands/output that we've already had the opportunity to report. Those commands can now be removed.
      oldIntersection = Map.intersection (fst3 $ _accum_stdout acc) (fst3 $ _accum_stderr acc)
  in Accum
      { _accum_stdout =
          ( (fst3 $ _accum_stdout acc') `Map.difference` oldIntersection -- Only include output of commands we haven't previously declared "done"
          , snd3 $ _accum_stdout acc'
          , thd3 $ _accum_stdout acc'
          )
      , _accum_stderr =
          ( (fst3 $ _accum_stderr acc') `Map.difference` oldIntersection -- Only include output of commands we haven't previously declared "done"
          , snd3 $ _accum_stderr acc'
          , thd3 $ _accum_stderr acc'
          )
      }

-- | Take all the pending output and consider it complete.
flushAccum :: Accum -> Accum
flushAccum (Accum (stdout, curout, stdoutLeftovers) (stderr, curerr, stderrLeftovers)) =
  Accum (Map.insert curout stdoutLeftovers stdout, curout+1, mempty) (Map.insert curerr stderrLeftovers stderr, curerr+1, mempty)

-- | Run a repl, feed it commands, and produce output grouped by those
-- commands. The repl in question must be able to print its prompt on both
-- stdout and stderr.
repl
  :: forall t m.
     ( Adjustable t m
     , MonadFix m
     , MonadHold t m
     , MonadIO (Performable m)
     , MonadIO m
     , NotReady t m
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     )
  => P.CreateProcess
  -- ^ Command to run to enter repl
  -> Event t [Command]
  -- ^ Commands to send to the repl
  -> (Int -> ByteString -> Bool)
  -- ^ Test for determining whether a line is the prompt we're waiting or
  -> m (Repl t)
repl runRepl cmds isPrompt = do
  let ix0 = 1
  n <- liftIO $ newIORef ix0
  newIxedInput <- performEvent $ ffor cmds $ \inputs -> do
    fmap Map.fromList $ forM inputs $ \input' -> do
      new_n <- liftIO $ atomicModifyIORef' n $ \n' -> (succ n', n')
      pure $ (new_n, input')
  ixedInput <- foldDyn Map.union Map.empty newIxedInput
  performEvent_ $ ffor (updated ixedInput) $ liftIO . appendFile "input" . show
  proc <- createProcess runRepl $ ProcessConfig
    { _processConfig_stdin = fmapMaybe sendCommands cmds
    , _processConfig_signal = never
    }
  performEvent_ $ ffor (_process_stderr proc) $ liftIO . C8.appendFile "stderr"
  pb <- getPostBuild
  exited <- performEventAsync $ ffor pb $ \_ cb ->
    liftIO $ void $ forkIO $ cb <=< P.waitForProcess $ _process_handle proc
  results <- foldDyn ($) (Accum (Map.empty, ix0, mempty) (Map.empty, ix0, mempty)) $ leftmost
    [ accumHandles isPrompt <$> align (_process_stdout proc) (_process_stderr proc)
    , flushAccum <$ exited
    ]
  let outerr = ffor results $ \(Accum o e) -> Map.intersectionWith (,) (fst3 o) (fst3 e)
  finished <- holdUniqDyn $ Map.intersectionWith (\inp (o, e) -> Cmd inp o e) <$> ixedInput <*> outerr
  let commandInProgress i (Accum o e) = (snd3 o,) $ case (Map.lookup (snd3 o) i) of
        Nothing -> Nothing
        Just inp -> Just $ Cmd inp (thd3 o) (thd3 e)
      started = commandInProgress <$> ixedInput <*> results
  pure $ Repl
    { _repl_process = proc
    , _repl_finished = updated finished
    , _repl_started = started
    , _repl_exited = exited
    }

-- * Output lines

-- | Accumulator for line-based output that keeps track of any dangling,
-- unterminated line
data Lines = Lines
  { _lines_terminated :: Seq C8.ByteString
  , _lines_unterminated :: Maybe C8.ByteString
  }
  deriving (Show, Eq, Ord, Read)

-- | Empty output
emptyLines :: Lines
emptyLines = Lines Seq.empty Nothing

-- | Add some raw output to a 'Lines'. This will chop the raw output up into lines.
addLines :: ByteString -> Lines -> Lines
addLines new (Lines t u) =
  let newLines = Seq.fromList $ filter (not . C8.null) (C8.lines new)
  in
    case u of
      Nothing -> if "\n" `C8.isSuffixOf` new
        then Lines (t <> newLines) Nothing
        else case Seq.viewr newLines of
                Seq.EmptyR -> Lines t Nothing
                (t' Seq.:> u') -> Lines (t <> t') (Just u')
      Just u' -> addLines (u' <> new) $ Lines t Nothing

-- | Convert a 'ByteString' into a 'Lines'
linesFromBS :: C8.ByteString -> Lines
linesFromBS = flip addLines mempty

instance Semigroup Lines where
  a <> b = addLines (unLines b) a

instance Monoid Lines where
  mempty = emptyLines

-- | Convert a 'Lines' back into a 'ByteString'
unLines :: Lines -> ByteString
unLines (Lines t u) =
  C8.unlines (toList t) <> fromMaybe "" u

-- | Convenience accessor for the last whole line received by a 'Lines'.
-- Ignores any unterminated line that may follow.
lastWholeLine :: Lines -> Maybe C8.ByteString
lastWholeLine (Lines t _) = case Seq.viewr t of
  Seq.EmptyR -> Nothing
  _ Seq.:> x -> Just x

-- | Split lines into two. The sequence that satisfies the predicate is
-- consumed and will not appear in either resulting 'Lines'.
splitLinesOn :: (ByteString -> Bool) -> Lines -> Maybe (Lines, Lines)
splitLinesOn test (Lines t u) = 
  let (before, after) = Seq.breakl test t
  in if Seq.null after then Nothing else Just (Lines before Nothing, Lines (Seq.drop 1 after) u)

-- * Commands to send to the repl

-- | A string that will be sent to the repl for evaluation. A newline will be
-- appended to the end of the string. 'Command's should not themselves contain newlines.
newtype Command = Command { unCommand :: ByteString }
  deriving (Show, Read, Eq, Ord)

-- | Constructs a 'Command' without checking for newlines. If there are
-- newlines in the input, things will not work properly.
unsafeCommand :: ByteString -> Command
unsafeCommand = Command

-- | Convert a 'ByteString' into a set of 'Command's. Any newlines found in the
-- input are considered 'Command' separators.
command :: ByteString -> [Command]
command = map Command . filter (not . C8.null) . C8.splitWith (=='\n')

-- | Convert a 'ByteString' into a set of 'Command's. Any newlines found in the
-- input are considered 'Command' separators.
commands :: [ByteString] -> [Command]
commands = map Command . filter (not . C8.null) . concatMap (C8.splitWith (=='\n'))

-- | Turn a command back into a 'ByteString'.
displayCommand :: Command -> ByteString
displayCommand = unCommand

-- | Convert commands to a format that can be sent over stdin
sendCommands :: [Command] -> Maybe (SendPipe ByteString)
sendCommands cmds = case cmds of
  [] -> Nothing
  xs -> Just $ SendPipe_Message (C8.intercalate "\n" (unCommand <$> xs) <> "\n")

-- * Misc

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

thd3 :: (a, b, c) -> c
thd3 (_, _, c) = c

-- * Testing

-- | A headless repl test that runs ghci, executes some commands, and checks that the output is as expected.
testRepl :: IO ()
testRepl = runHeadlessApp $ do
  _ <- liftIO $ Signals.installHandler Signals.sigINT (Signals.Catch (return ())) Nothing
  pb <- getPostBuild
  testCommands <- mkTestCommands
  let cmds = mergeWith (\a b -> a <> b)
        [ command (C8.intercalate "\n"
            [ ":set prompt-function \\_ x -> let s = \"\\n\" <> show x <> \"" <> promptPostfix <> "\\n\" in System.IO.hPutStr System.IO.stderr s >> pure s"
            , ":set -fno-break-on-exception"
            , ":set -fno-break-on-error"
            , ":set -v1"
            , ":set -fno-hide-source-paths"
            , ":set -ferror-spans"
            , ":set -fdiagnostics-color=never"
            , ":r"
            ]) <$ pb
        , testCommands
        ]
  rec (Repl _ finished _ exit) <- repl (P.shell "ghci") cmds $ \cur line -> (C8.pack (show cur) <> promptPostfix) == line
  output <- foldDyn Map.union Map.empty finished
  passed <- performEvent $ ffor (tagPromptlyDyn output exit) $ \o -> do
    liftIO $ putStrLn "testRepl:"
    r1 <- assertStdoutEq "Simple command (1+1)" (Map.lookup 9 o) "2"
    r2 <- assertStdoutEq "IO action (putStrLn)" (Map.lookup 10 o) "hello"
    r3 <- assertStderr "Not in scope error" (Map.lookup 11 o) (C8.isInfixOf "error: Variable not in scope: oops" . unLines)
    r4 <- assertStdoutEq "Simple command (2+2)" (Map.lookup 12 o) "4"
    r5 <- assertStdoutEq "Simple command (3+4)" (Map.lookup 13 o) "7"
    r6 <- assertStderr "Exception" (Map.lookup 14 o) (C8.isPrefixOf "*** Exception" . unLines)
    r7 <- assertStdoutEq "Reload (:r)" (Map.lookup 15 o) "Ok, no modules loaded."
    r8 <- assertStdoutEq "Quit (:q)" (Map.lookup 16 o) "Leaving GHCi."
    pure $ and [r1, r2, r3, r4, r5, r6, r7, r8]
  performEvent $ ffor passed $ \case
    False -> error "Test failed"
    True -> pure ()
  where
    promptPostfix :: ByteString
    promptPostfix = "_reflex_ghci_prompt>"

-- | Constructs some testing commands that are fed in on a timer
mkTestCommands :: (PerformEvent t m, PostBuild t m, TriggerEvent t m, MonadIO (Performable m)) => m (Event t [Command])
mkTestCommands = do
  pb <- getPostBuild
  pb2 <- delay 1 pb
  pb3 <- delay 1.5 pb
  pb4 <- delay 2 pb
  pb5 <- delay 2.5 pb
  pb6 <- delay 3 pb
  pure $ fmap command $ mergeWith (\a b -> a <> "\n" <> b)
    [ "1+1" <$ pb2
    , "putStrLn \"hello\"" <$ pb3
    , "oops" <$ pb4
    , "2+2" <$ pb5
    , "4+3" <$ pb5
    , "let Just x = Nothing in print x" <$ pb5
    , ":r" <$ pb6
    , ":q" <$ pb6
    ]

-- | Check that stdout equals the given value
assertStdoutEq :: MonadIO m => String -> Maybe Cmd -> ByteString -> m Bool
assertStdoutEq str cmd expectation = assertHandleEq _cmd_stdout str cmd expectation

-- | Check that stderr equals the given value
assertStderrEq :: MonadIO m => String -> Maybe Cmd -> ByteString -> m Bool
assertStderrEq str cmd expectation = assertHandleEq _cmd_stderr str cmd expectation

-- | Test the contents of stderr
assertStderr :: MonadIO m => String -> Maybe Cmd -> (Lines -> Bool) -> m Bool
assertStderr = assertHandle _cmd_stderr

-- | Test the contents of stdout
assertStdout :: MonadIO m => String -> Maybe Cmd -> (Lines -> Bool) -> m Bool
assertStdout = assertHandle _cmd_stdout

-- | Check that a handle equals the given value
assertHandleEq :: MonadIO m => (Cmd -> Lines) -> String -> Maybe Cmd -> ByteString -> m Bool
assertHandleEq h str cmd expectation = assertHandle h str cmd (== Lines (Seq.singleton expectation) Nothing)

-- | Test the contents of a handle
assertHandle :: MonadIO m => (Cmd -> Lines) -> String -> Maybe Cmd -> (Lines -> Bool) -> m Bool
assertHandle h str cmd expectation = assertCmd str cmd (expectation . h)

-- | Test that a repl command and its output satisfy the predicate
assertCmd :: MonadIO m => String -> Maybe Cmd -> (Cmd -> Bool) -> m Bool
assertCmd str cmd expectation = liftIO $ do
  putStrLn $ "Testing: " <> str
  if ((expectation <$> cmd) == Just True)
    then do
      putStrLn $ "PASSED: " <> str
      pure True
    else do
      putStrLn $ "FAILED: " <> str
      pure False
