{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Reflex
import Reflex.Host.Headless (runHeadlessApp)
import Reflex.Workflow (Workflow (..), workflow)
import System.Directory (copyFile, getCurrentDirectory, withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P

import Reflex.Process.GHCi
import Reflex.Vty.GHCi

ghciExe :: FilePath
ghciExe = "ghci"

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

main :: IO ()
main = do
  src <- getCurrentDirectory
  let cmd path load = P.proc ghciExe ["-i" <> src <> path, load]
  putStrLn "Testing lib-pkg"
  testLoadAndExecute $ cmd "/tests/lib-pkg/src" "MyLib"
  putStrLn "Testing lib-exe"
  testLoadAndExecute $ cmd "/tests/exe-pkg" "Main"
  putStrLn "Testing lib-pkg-err"
  runHeadlessApp $ do
    out <- testModuleLoadFailed $ cmd "/tests/lib-pkg-err/src" "MyLib"
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
testLoadAndExecute cmd = runHeadlessApp $ do
  out <- switch . current <$> workflow (testModuleLoad cmd)
  performEvent_ $ liftIO . print <$> out
  failOnError out
  exitOnSuccess out

type TestWorkflow t m = Workflow t m (Event t ExitStatus)

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
  g <- ghciWatch cmd Nothing
  let loaded = fforMaybe (updated $ _ghci_status g) $ \case
        Status_LoadSucceeded -> Just True
        Status_LoadFailed -> Just False
        _ -> Nothing
  done <- afterShutdown g loaded
  return ( Failed . show <$> ffilter not done
         , testExprErr cmd <$ ffilter id done
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
  g <- ghciWatch cmd $ Just "err"
  let exception = fforMaybe (updated $ _ghci_status g) $ \case
        Status_ExecutionFailed -> Just True
        Status_ExecutionSucceeded -> Just False
        _ -> Nothing
  done <- afterShutdown g exception
  return ( Failed . show <$> ffilter not done
         , testExprNotFound cmd <$ ffilter id done
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
  g <- ghciWatch cmd $ Just "notTheFunctionYoureLookingFor"
  let exception = fforMaybe (updated $ _ghci_status g) $ \case
        Status_ExecutionFailed -> Just True
        Status_ExecutionSucceeded -> Just False
        _ -> Nothing
  done <- afterShutdown g exception
  return ( Failed . show <$> ffilter not done
         , testExprFinished cmd <$ ffilter id done
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
  g <- ghciWatch cmd $ Just "done"
  let finished = fforMaybe (updated $ _ghci_status g) $ \case
        Status_ExecutionFailed -> Just False
        Status_ExecutionSucceeded -> Just True
        _ -> Nothing
  done <- afterShutdown g finished
  return ( outcome done
         , never
         )

outcome :: Functor f => f Bool -> f ExitStatus
outcome = fmap (\x -> if x then Succeeded else Failed (show x))

watchAndReloadTest :: IO ()
watchAndReloadTest = withSystemTempDirectory "reflex-ghci-test" $ \p -> do
  src <- getCurrentDirectory
  P.callProcess "cp" ["-r", src <> "/tests/lib-pkg-err", p]
  let cmd = P.proc ghciExe ["-i" <> p <> "/lib-pkg-err/src", "MyLib"]
  withCurrentDirectory p $ runHeadlessApp $ do
    g <- ghciWatch cmd Nothing
    performEvent_ $ ffor (_ghci_moduleOut g) $ liftIO . print
    performEvent_ $ ffor (_ghci_moduleErr g) $ liftIO . print
    performEvent_ $ ffor (updated $ _ghci_status g) $ liftIO . print
    let loadFailed = fforMaybe (updated $ _ghci_status g) $ \case
          Status_LoadFailed -> Just ()
          _ -> Nothing

    loadFailedDelayed <- delay 1 loadFailed -- If we respond too quickly, fsnotify won't deliver the change.
    performEvent_ $ ffor loadFailedDelayed $ \_ -> liftIO $ do
      putStrLn "copying fixed file"
      copyFile (src <> "/tests/lib-pkg/src/MyLib/Three.hs")
               (p <> "/lib-pkg-err/src/MyLib/Three.hs")

    numFailures :: Dynamic t Int <- count loadFailed
    let loadSucceeded = fforMaybe (updated $ _ghci_status g) $ \case
          Status_LoadSucceeded -> Just ()
          _ -> Nothing
    performEvent_ $ fforMaybe (updated numFailures) $ \x ->
      if x > 1
        then Just $ error "Too many failures."
        else Nothing

    afterShutdown g $ gate ((>= 1) <$> current numFailures) loadSucceeded

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
  -> m (Event t ExitStatus)
testModuleLoadFailed cmd = do
  liftIO $ putStrLn "testModuleLoadFailed"
  g <- ghciWatch cmd Nothing
  let loaded = fforMaybe (updated $ _ghci_status g) $ \case
        Status_LoadSucceeded -> Just False
        Status_LoadFailed -> Just True
        _ -> Nothing
  done <- afterShutdown g loaded
  return $ ffor done $ \x ->
    if x
      then Succeeded
      else Failed "testModuleFailed: lib-pkg-err shouldn't have loaded"

-- | Utility to help use an 'Event' for stopping the network, but not until the shutdown is complete.
afterShutdown :: (PerformEvent t m, MonadIO (Performable m), MonadHold t m) => Ghci t -> Event t b -> m (Event t b)
afterShutdown g e = do
  eValue <- hold Nothing $ Just <$> e
  after <- shutdown $ g <$ e
  pure $ fmapMaybe id $ tag eValue after
