{-|
 - Module: Reflex.Vty.GHCi
 - Description: Vty widgets useful when building your own GHCi runner
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Reflex.Vty.GHCi where

import Control.Monad ((<=<), void)
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Reflex.Network
import Reflex.Process
import Reflex.Process.GHCi
import Reflex.Vty
import qualified Graphics.Vty.Input as V
import qualified System.Process as P

-- | Display the overall status of the GHCi session, including exit information in case GHCi has quit
statusDisplay
  :: ( PostBuild t m
     , MonadHold t m
     )
  => Ghci t
  -> VtyWidget t m ()
statusDisplay g = do
  pb <- getPostBuild
  text <=< hold "" $ leftmost
    [ statusMessage <$> updated (_ghci_status g)
    , statusMessage <$> tag (current $ _ghci_status g) pb
    , ("Command exited with " <>) . T.pack . show <$> _process_exit (_ghci_process g)
    ]

-- | A scrollable widget that displays a message at the bottom of the widget
-- when there is additional content to view.
scrollableOutput
  :: ( Reflex t
     , MonadNodeId m
     , MonadHold t m
     , MonadFix m
     , PostBuild t m
     )
  => Behavior t ByteString
  -> VtyWidget t m ()
scrollableOutput out = col $ do
  dh <- displayHeight
  scroll <- stretch $ scrollableText never $ T.decodeUtf8 <$> out
  fixed 1 $ text $
    let f h (ix, n) = if n - ix + 1 > h
          then "↓ More ↓"
          else ""
    in f <$> current dh <*> scroll

-- | A scrollable widget that scrolls down as output goes past the end of the widget
scrollingOutput
  :: ( Reflex t
     , Monad m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t ByteString
  -> VtyWidget t m ()
scrollingOutput out = do
  dh <- displayHeight
  let scrollBy h (ix, n) =
        if | ix == 0 && n <= h -> Nothing -- Scrolled to the top and we don't have to scroll down
           | n > h && n - ix - h == 0 -> Just 1
           | otherwise -> Nothing
  rec scroll <- scrollableText (tagMaybe (scrollBy <$> current dh <*> scroll) $ updated out) $
        T.decodeUtf8 <$> current out
  return ()

-- | Display the output GHCi produces when it's loading the requested modules (e.g., warnings)
ghciModuleStatus
  :: ( MonadNodeId m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Adjustable t m
     )
  => Ghci t
  -> VtyWidget t m ()
ghciModuleStatus g = col $ do
  let ghciExit = _process_exit $ _ghci_process g
  ghciExited <- hold False $ True <$ ghciExit
  fixed 3 $ boxStatic def $ statusDisplay g
  out <- moduleOutput (not <$> ghciExited) g
  stretch $ void $
    networkHold (scrollableOutput $ current out) $ ffor (_ghci_reload g) $
      const $ scrollableOutput $ current out

-- | Display the output of the expression GHCi is evaluating
ghciExecOutput
  :: ( MonadHold t m
     , MonadFix m
     , Adjustable t m
     )
  => Ghci t
  -> VtyWidget t m ()
ghciExecOutput g = do
  ghciExited <- hold False $ True <$ _process_exit (_ghci_process g)
  out <- execOutput (not <$> ghciExited) g
  -- Rebuild the entire output widget so that we don't have to worry about resetting scroll state
  _ <- networkHold (scrollingOutput out) $ ffor (_ghci_reload g) $ \_ -> scrollingOutput out
  return ()

-- | A widget that displays the module status and the execution status in two stacked, resizable panes
ghciPanes
  :: ( Reflex t
     , MonadFix m
     , MonadHold t m
     , MonadNodeId m
     , PostBuild t m
     , Adjustable t m
     )
  => Ghci t
  -> VtyWidget t m ()
ghciPanes g = void $ splitVDrag
  (hRule doubleBoxStyle)
  (ghciModuleStatus g)
  (ghciExecOutput g)

-- | Listen for ctrl-c (and any other provided exit events) and
-- shutdown the Ghci process upon receipt
getExitEvent
  :: ( PerformEvent t m
     , MonadIO (Performable m)
     )
  => Ghci t
  -> Event t a
  -> VtyWidget t m (Event t ())
getExitEvent g externalExitReq = do
  exitReq <- keyCombo (V.KChar 'c', [V.MCtrl])
  let exitReqs = leftmost
        [ g <$ externalExitReq
        , g <$ exitReq
        ]
  shutdown exitReqs

-- | Shut down a given Ghci process
shutdown
  :: ( PerformEvent t m
     , MonadIO (Performable m)
     )
  => Event t (Ghci t)
  -> m (Event t ())
shutdown exitReqs = do
  performEvent $ ffor exitReqs $ \g ->
    liftIO $ P.terminateProcess $ _process_handle $ _ghci_process g
