{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
import Reflex
import Reflex.Process.GHCi
import Reflex.Vty
import Reflex.Vty.GHCi
import Reflex.Workflow

import Control.Monad (void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import qualified System.Process as P
import System.Directory
import System.IO.Temp

data ExitStatus = Succeeded | Failed String
  deriving (Eq, Show)

-- Simple tests with no reloading or interrupts:
--
-- | Test                 | Module Status         | Expression Status         |
-- |----------------------|-----------------------|---------------------------|
-- | testModuleLoad       | Status_LoadSucceeded  | None                      |
-- | testModuleLoadFailed | Status_LoadFailed     | None                      |
-- | testExprErr          | Status_LoadSucceeded  | Status_ExecutionFailed    |
-- | testExprNotFound     | Status_LoadSucceeded  | Status_ExecutionFailed    |
-- | testExprFinished     | Status_LoadSucceeded  | Status_ExecutionSucceeded |

ghciInterruptible
  :: ( TriggerEvent t m
     , PerformEvent t m
     , Reflex t
     , PostBuild t m
     , MonadIO (Performable m)
     , MonadIO m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> Maybe ByteString
  -> VtyWidget t m (Ghci t)
ghciInterruptible cmd mexpr = do
  g <- ghciWatch cmd mexpr
  e <- getExitEvent g never
  performEvent_ $ ffor e $ \_ -> error "Interrupted."
  return g

main :: IO ()
main = do
  let cmd path = (P.proc "cabal" ["repl"]) { P.cwd = Just path }
  putStrLn "Testing lib-pkg"
  testLoadAndExecute $ cmd "./tests/lib-pkg"
  putStrLn "Testing lib-exe"
  testLoadAndExecute $ cmd "./tests/exe-pkg"
  putStrLn "Testing lib-pkg-err"
  mainWidget $ do
    out <- testModuleLoadFailed $ cmd "./tests/lib-pkg-err"
    failOnError out
    exitOnSuccess out
  putStrLn "Testing file watching and reloading"
  watchAndReloadTest

failOnError
  :: ( PerformEvent t m
     , Reflex t
     )
  => Event t ExitStatus -> m ()
failOnError out = performEvent_ $ fforMaybe out $ \case
  Failed err -> Just $ error $ "Failed with: " <> err
  Succeeded -> Nothing

exitOnSuccess
  :: ( Reflex t
     , Monad m
     )
  => Event t ExitStatus
  -> m (Event t ())
exitOnSuccess out =
  return $ () <$ ffilter (==Succeeded) out

testLoadAndExecute
  :: P.CreateProcess
  -> IO ()
testLoadAndExecute cmd = mainWidget $ do
  out <- switch . current <$> workflow (testModuleLoad cmd)
  performEvent_ $ liftIO . print <$> out
  failOnError out
  exitOnSuccess out

type TestWorkflow t m = Workflow t (VtyWidget t m) (Event t ExitStatus)

testModuleLoad
  :: ( MonadIO m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> TestWorkflow t m
testModuleLoad cmd = Workflow $ do
  liftIO $ putStrLn "testModuleLoad"
  g <- ghciInterruptible cmd Nothing
  let loaded = fforMaybe (updated $ _ghci_status g) $ \case
        Status_LoadSucceeded -> Just True
        Status_LoadFailed -> Just False
        _ -> Nothing
  void $ shutdown $ g <$ loaded
  return ( Failed . show <$> ffilter not loaded
         , testExprErr cmd <$ ffilter id loaded
         )

testExprErr
  :: ( MonadIO m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> TestWorkflow t m
testExprErr cmd = Workflow $ do
  liftIO $ putStrLn "testExprErr"
  g <- ghciInterruptible cmd $ Just "err"
  let exception = fforMaybe (updated $ _ghci_status g) $ \case
        Status_ExecutionFailed -> Just True
        Status_ExecutionSucceeded -> Just False
        _ -> Nothing
  void $ shutdown $ g <$ exception
  return ( Failed . show <$> ffilter not exception
         , testExprNotFound cmd <$ ffilter id exception
         )

testExprNotFound
  :: ( MonadIO m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> TestWorkflow t m
testExprNotFound cmd = Workflow $ do
  liftIO $ putStrLn "testExprNotFound"
  g <- ghciInterruptible cmd $ Just "notTheFunctionYoureLookingFor"
  let exception = fforMaybe (updated $ _ghci_status g) $ \case
          Status_ExecutionFailed -> Just True
          Status_ExecutionSucceeded -> Just False
          _ -> Nothing
  void $ shutdown $ g <$ exception
  return ( Failed . show <$> ffilter not exception
         , testExprFinished cmd <$ ffilter id exception
         )

testExprFinished
  :: ( MonadIO m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> TestWorkflow t m
testExprFinished cmd = Workflow $ do
  liftIO $ putStrLn "testExprFinished"
  g <- ghciInterruptible cmd $ Just "done"
  let done = fforMaybe (updated $ _ghci_status g) $ \case
          Status_ExecutionFailed -> Just False
          Status_ExecutionSucceeded -> Just True
          _ -> Nothing
  void $ shutdown $ g <$ done
  return ( outcome done
         , never
         )

outcome :: Functor f => f Bool -> f ExitStatus
outcome = fmap (\x -> if x then Succeeded else Failed (show x))

watchAndReloadTest :: IO ()
watchAndReloadTest = withSystemTempDirectory "reflex-ghci-test" $ \p -> do
  src <- getCurrentDirectory
  P.callProcess "cp" ["-r", src <> "/tests/lib-pkg-err", p]
  let cmd = (P.proc "cabal" ["repl"])
        { P.cwd = Just (p <> "/lib-pkg-err") }
  withCurrentDirectory p $ mainWidget $ do
    liftIO $ putStrLn $ "Running main widget"
    g <- ghciInterruptible cmd Nothing
    liftIO $ putStrLn $ "Started ghci"
    performEvent_ $ ffor (_ghci_moduleOut g) $ liftIO . print
    performEvent_ $ ffor (_ghci_moduleErr g) $ liftIO . print
    performEvent_ $ ffor (updated $ _ghci_status g) $ liftIO . print
    let loadFailed = fforMaybe (updated $ _ghci_status g) $ \case
          Status_LoadFailed -> Just ()
          _ -> Nothing
    numFailures :: Dynamic t Int <- count loadFailed
    performEvent_ $ ffor loadFailed $ \_ -> liftIO $ do
      putStrLn "copying fixed file"
      P.callProcess "cp"
        [ src <> "/tests/lib-pkg/src/MyLib/Three.hs"
        , p <> "/lib-pkg-err/src/MyLib/Three.hs"
        ]
    let loadSucceeded = fforMaybe (updated $ _ghci_status g) $ \case
          Status_LoadSucceeded -> Just ()
          _ -> Nothing
    performEvent_ $ fforMaybe (updated numFailures) $ \x ->
      if x > 1
        then Just $ error "Too many failures."
        else Nothing
    return loadSucceeded

testModuleLoadFailed
  :: ( MonadIO m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => P.CreateProcess
  -> VtyWidget t m (Event t ExitStatus)
testModuleLoadFailed cmd = do
  liftIO $ putStrLn "testModuleLoadFailed"
  g <- ghciInterruptible cmd Nothing
  let loaded = fforMaybe (updated $ _ghci_status g) $ \case
        Status_LoadSucceeded -> Just False
        Status_LoadFailed -> Just True
        _ -> Nothing
  void $ shutdown $ g <$ loaded
  return $ ffor loaded $ \x ->
    if x
      then Succeeded
      else Failed "testModuleFailed: lib-pkg-err shouldn't have loaded"
