{-|
 - Module: Reflex.Vty.GHCi
 - Description: Vty widgets useful when building your own GHCi runner
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Reflex.Vty.GHCi where

import Control.Monad
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Graphics.Vty.Input as V
import Reflex.Network
import Reflex.Process.GHCi
import Reflex.Vty
import qualified System.Process as P
import qualified Reflex.Process.Repl as Repl

-- | The main reflex-ghci widget
run :: String -> Maybe String -> IO ()
run cmd expr = mainWidget $ initManager_ $ do
  tabNavigation
  exit' <- keyCombo (V.KChar 'c', [V.MCtrl])
  exit <- keyCombo (V.KChar 'x', [V.MCtrl])
  rec Repl _ finished started readyToExit <- ghciWatch (P.shell cmd) (Repl.unsafeCommand . T.encodeUtf8 . T.pack <$> expr) reload ((() <$ exit) <> (() <$ exit') <> quit)
      oldCommands <- foldDyn ($) Map.empty $
        (\new old -> Map.fromList $ take 3 $ reverse $ Map.toList $ Map.union old new) <$> finished
      command <- holdDyn Nothing $ fmap Just $ leftmost
        [ fforMaybe (updated started) $ \(ix, x) -> (ix,) <$> x
        , fmapMaybe id $ fmap fst . Map.maxViewWithKey <$> finished
        ]
      let atPrompt = ffor started $ isNothing . snd
          errors = maybe False (hasErrors . snd) <$> command
      (reload, quit) <- col $ do
        r <- tile (fixed 3) $ row $ do
          grout flex $ boxStatic def $ text $ (\x -> if x then "Ready" else "Busy") <$> current atPrompt
          void $ networkView $ ffor errors $ \case
            True -> grout flex $ boxStatic def $ text "Error!"
            False -> grout flex $ boxStatic def $ text "All Good!"
          r <- tile flex $ button def $ text "Reload"
          q <- tile flex $ button def $ text "Quit"
          pure (r, q)
        let cmdbtn ix c = tile (fixed 3) $ button def $ text $ pure $
              T.pack (show ix) <> ": " <> (T.decodeUtf8 $ displayCommand $ _cmd_stdin c)
        oldE <- switchHold never <=< fmap (fmap leftmost) $ networkView $ ffor oldCommands $ \old -> forM (Map.toList old) $ \(ix, c) -> do
          go <- cmdbtn ix c
          pure $ ix <$ go
        currentCommand <- switchHold never <=< networkView $ ffor started $ \case
          (_, Nothing) -> pure never
          (ix, Just c) -> cmdbtn ix c

        let showOutput (Cmd _ out err) = do
              let scrollCfg = def
                    { _scrollableConfig_startingPosition = ScrollPos_Bottom
                    , _scrollableConfig_scrollToBottom = pure (Just ScrollToBottom_Maintain)
                    }
              _ <- tile flex $ boxStatic def $ scrollableText scrollCfg $ pure $ T.decodeUtf8 . unLines $ out
              _ <- tile flex $ boxStatic def $ scrollableText scrollCfg $ pure $ T.decodeUtf8 . unLines $ err
              pure ()

        void $ networkHold (void $ networkView $ maybe blank (showOutput . snd) <$> command) $ leftmost
          [ ffor (attachWithMaybe (flip Map.lookup) (current oldCommands) oldE) showOutput
          , ffor (attachWithMaybe (\a _ -> fmap snd a) (current command) currentCommand) showOutput
          ]
        pure r
  return $ () <$ readyToExit
