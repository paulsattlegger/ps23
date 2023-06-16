{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (Widget)
import Brick.Types qualified as T
import Brick.Widgets.Core (str)
import Control.Monad (void)
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib

newtype EditorState n = EditorState {_text :: String}

makeLenses ''EditorState

drawUI :: EditorState () -> [Widget ()]
drawUI p = [str $ p ^. text]

appEvent :: T.BrickEvent () e -> T.EventM () (EditorState ()) ()
appEvent (T.VtyEvent e) = keyEvent e
appEvent _ = return ()

keyEvent :: V.Event -> T.EventM () (EditorState ()) ()
keyEvent (V.EvKey (V.KChar c) []) = do
  text %= (++ [c])
keyEvent (V.EvKey V.KBS []) = do
  text %= init2
keyEvent (V.EvKey V.KEsc []) = M.halt
keyEvent _ = return ()

initialState :: EditorState ()
initialState = EditorState ""

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr []

editor :: M.App (EditorState ()) e ()
editor =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

main :: IO ()
main = void $ M.defaultMain editor initialState
