{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
import Reflex.Network
import Reflex.Process
import Reflex.Process.GHCi
import Reflex.Vty

import Control.Concurrent (threadDelay)
import Control.Monad (void, (<=<))
import qualified Graphics.Vty.Input as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Environment (getArgs)
import System.Process (shell)

main :: IO ()
main = do
  [cmd, expr] <- getArgs
  mainWidget $ do
    exit <- keyCombo (V.KChar 'c', [V.MCtrl])
    let mexpr = if null expr then Nothing else Just $ T.encodeUtf8 $ T.pack expr
    g <- ghciWatch (shell cmd) mexpr
    pb <- getPostBuild
    let ghciExit = _process_exit (_ghci_process g)
    ghciExited <- hold False $ True <$ ghciExit
    let ghciLoadStatus = col $ do
          fixed 3 $ boxStatic def $ text <=< hold "" $ leftmost
            [ statusMessage <$> updated (_ghci_status g)
            , statusMessage <$> tag (current $ _ghci_status g) pb
            , ("GHCi exited with " <>) . T.pack . show <$> ghciExit
            ]
          out <- moduleOutput (not <$> ghciExited) g
          (dh, scroll) <- stretch $ do
            dh <- displayHeight
            scroll <- scrollableText never $ T.decodeUtf8 <$> current out
            return (dh, scroll)
          fixed 1 $ text $ (\h (ix, n) -> if n - ix + 1 > h then "↓ More ↓" else "") <$> current dh <*> scroll
        ghciExecOutput = do
          out <- (T.decodeUtf8 <$>) <$> execOutput (not <$> ghciExited) g
          let scrollingOutput = do
                dh <- displayHeight
                rec scroll <- scrollableText (tagMaybe (scrollBy <$> current dh <*> scroll) $ updated out) $ current out
                    let scrollBy h (ix, n) =
                          if | ix == 0 && n <= h -> Nothing -- Scrolled to the top and we don't have to scroll down
                             | n > h && n - ix - h == 0 -> Just 1
                             | otherwise -> Nothing
                return ()
          -- Rebuild the entire output widget so that we don't have to worry about resetting scroll state
          _ <- networkHold scrollingOutput $ ffor (_ghci_reload g) $ \_ -> scrollingOutput
          return ()
    case mexpr of
      Nothing -> ghciLoadStatus
      Just _ -> void $ splitVDrag (hRule doubleBoxStyle) ghciLoadStatus ghciExecOutput
    return $ () <$ exit

test :: IO ()
test = do
  let go :: Int -> IO ()
      go n = putStrLn ("Iteration No. " <> show n) >> threadDelay 1000000 >> go (n+1)
  -- error "asdf"
  -- Just n <- return (Nothing :: Maybe Int)
  go 1
