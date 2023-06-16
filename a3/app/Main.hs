{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types (Widget)
import Brick.Types qualified as T
import Brick.Util (on)
import Brick.Widgets.Core (strWrap)
import Control.Monad (void)
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib

data EditorState n = EditorState {_text :: String, _index :: Int}

makeLenses ''EditorState

drawUI :: EditorState () -> [Widget ()]
drawUI s = [strWrap (before ++ "_" ++ after)]
  where
    (before, after) = splitAt (s ^. index) (s ^. text)

appEvent :: T.BrickEvent () e -> T.EventM () (EditorState ()) ()
appEvent (T.VtyEvent (V.EvKey e [])) = keyEvent e
appEvent _ = return ()

keyEvent :: V.Key -> T.EventM () (EditorState ()) ()
keyEvent (V.KChar c) = do
  cursor <- use index
  text %= \i -> insert' cursor c i
  index += 1
keyEvent V.KEnter = do
  cursor <- use index
  text %= \i -> insert' cursor '\n' i
  index += 1
keyEvent V.KBS = do
  cursor <- use index
  text %= \i -> delete' cursor i
  index %= \i -> max 0 (i - 1)
keyEvent V.KEsc = M.halt
keyEvent V.KRight = do
  textLength <- use (text . to length)
  index %= \i -> min (i + 1) textLength
keyEvent V.KLeft = do
  index %= \i -> max 0 (i - 1)
keyEvent _ = return ()

initialState :: EditorState ()
initialState = EditorState "" 0

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
