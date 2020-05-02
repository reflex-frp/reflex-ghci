import Reflex.Process
import Reflex.Process.GHCi
import Reflex.Vty
import Reflex.Vty.GHCi

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Version (showVersion)
import qualified Graphics.Vty.Input as V
import Options.Applicative
import qualified Options.Applicative as OP (switch)
import Paths_reflex_ghci (version)
import System.Process (shell, terminateProcess)

data GhciArg = GhciArg
  { _ghciArg_replCommand :: String
  , _ghciArg_execCommand :: Maybe String
  , _ghciArg_diagnosticsColor :: Maybe Bool
  }

ghciArg :: Parser GhciArg
ghciArg = GhciArg
  <$> strOption
    ( long "command" <>
      short 'c' <>
      help "The ghci/cabal repl command to run" <>
      showDefault <>
      value "cabal repl --repl-options=-Wall" <>
      metavar "COMMAND"
    )
  <*> optional (strOption
    ( long "expression" <>
      short 'e' <>
      help "The optional expression to evaluate once modules have successfully loaded" <>
      metavar "EXPR"
    ))
  <*> optional (OP.switch
    ( long "color" <>
      help "The option to set colored output via -fdiagnostics-color=always"
    ))

main :: IO ()
main = do
  let opts = info (ghciArg <**> helper) $ mconcat
        [ fullDesc
        , progDesc "Run a Haskell REPL that automatically reloads when source files change."
        , header $ "Welcome to reflex-ghci " <>
            showVersion version
        ]
  GhciArg { _ghciArg_replCommand = cmd, 
            _ghciArg_execCommand = expr, 
            _ghciArg_diagnosticsColor = color } <- execParser opts
  let ghciOpts = if color == Just True then [ C8.pack ":set -fdiagnostics-color=always" ] else []
  mainWidget $ do
    exit <- keyCombo (V.KChar 'c', [V.MCtrl])
    g <- ghciWatch (shell cmd) ghciOpts $ T.encodeUtf8 . T.pack <$> expr
    case expr of
      Nothing -> ghciModuleStatus g
      Just _ -> ghciPanes g
    readyToExit <- performEvent $ ffor exit $ \_ -> liftIO $ terminateProcess $ _process_handle $ _ghci_process g
    return $ () <$ readyToExit

-- Some rudimentary test expressions
-- Run these to test different scenarios like so:
--
-- > reflex-ghci "cabal repl exe:reflex-ghci" "test"
--
test :: IO ()
test = do
  let go :: Int -> IO ()
      go n = putStrLn ("Iteration No. " <> show n) >> threadDelay 1000000 >> go (n+1)
  go 1

err :: IO ()
err = error "This is an error"

err2 :: IO ()
err2 = do
  Just _ <- return (Nothing :: Maybe Int)
  test

done :: IO ()
done = putStrLn "Done"
