{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

import Control.Monad
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Reflex
import Reflex.Host.Headless (runHeadlessApp)
import System.Directory (copyFile, getCurrentDirectory, withCurrentDirectory)
import System.IO.Temp (withSystemTempDirectory)
import qualified System.Process as P
import qualified Data.ByteString.Char8 as C8

import Reflex.Process.GHCi

ghciExe :: FilePath
ghciExe = "ghci"

data ExitStatus = Succeeded | Failed String
  deriving (Eq, Show)

main :: IO ()
main = do
  runHeadlessApp $ do
    pb <- getPostBuild
    rec
      r <- ghci (P.shell ghciExe) expr never never
      let expr = commands 
            [ ":l tests/exe-pkg/Main.hs"
            , "main"
            , ":l tests/lib-pkg-err/src/MyLib/One.hs tests/lib-pkg-err/src/MyLib/Two.hs tests/lib-pkg-err/src/MyLib/Three.hs tests/lib-pkg-err/src/MyLib.hs"
            , ":l tests/lib-pkg/src/MyLib/One.hs tests/lib-pkg/src/MyLib/Two.hs tests/lib-pkg/src/MyLib/Three.hs tests/lib-pkg/src/MyLib.hs"
            , "import MyLib"
            , "err"
            , "notFound"
            , "done"
            , ":q"
            ] <$ pb
      results <- foldDyn Map.union Map.empty $ _repl_finished r
      performEvent_ $ ffor (tagPromptlyDyn results (_repl_exited r)) $ \o -> do
        liftIO $ print o
        liftIO $ putStrLn "Simple tests with no reloading or interrupts:"
        r1 <- assertStdoutEq "Load and run package (exe-pkg/Main.hs:main)" (Map.lookup 10 o) "Hello, Haskell!"
        r2 <- assertStderr "Fail to load package (lib-pkg-err)" (Map.lookup 11 o) (C8.isInfixOf "Multiple declarations" . unLines)
        r3 <- assertStdout "Load package (lib-pkg)" (Map.lookup 12 o) (C8.isInfixOf "Ok, four modules loaded" . unLines)
        r4 <- assertCmd "Exception (lib-pkg/src/MyLib.hs:err)" (Map.lookup 14 o) hasErrors
        r5 <- assertCmd "Variable not in scope (lib-pkg/src/MyLib.hs:notFound)" (Map.lookup 15 o) hasErrors
        r6 <- assertStdoutEq "Expression completed (lib-pkg/src/MyLib.hs:done)" (Map.lookup 16 o) "done"
        when (not . and $ [r1, r2, r3, r4, r5, r6]) $ error "Tests failed"
    pure $ () <$ _repl_exited r
  testRepl
  watchAndReloadTest

watchAndReloadTest :: IO ()
watchAndReloadTest = withSystemTempDirectory "reflex-ghci-test" $ \p -> do
  putStrLn "watchAndReloadTest:"
  src <- getCurrentDirectory
  P.callProcess "cp" ["-r", src <> "/tests/lib-pkg-err", p]
  let cmd = P.proc ghciExe ["-i" <> p <> "/lib-pkg-err/src", "MyLib"]
  withCurrentDirectory p $ runHeadlessApp $ do
    g <- ghciWatch cmd (Just (unsafeCommand ":q")) never never
    loadFailedDelayed <- debounce 0.5 $ ffilter id $ updated $ ffor (_repl_started g) $ \case
      (_, Nothing) -> True
      _ -> False

    performEvent_ $ ffor loadFailedDelayed $ \_ -> liftIO $ do
      putStrLn "copying fixed file"
      copyFile (src <> "/tests/lib-pkg/src/MyLib/Three.hs")
               (p <> "/lib-pkg-err/src/MyLib/Three.hs")

    output <- foldDyn Map.union Map.empty $ _repl_finished g

    performEvent $ ffor (tagPromptlyDyn output (_repl_exited g)) $ \o -> do
      r1 <- assertCmd "Failed to load file" (Map.lookup 8 o) hasErrors
      r2 <- assertCmd "Changed file detected and loaded" (Map.lookup 9 o) (not . hasErrors)
      when (not . and $ [r1, r2]) $ error "Test failed"
