{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Util (on)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (str, strWrap, withAttr, (<=>), (<+>))
import Control.Monad (void)
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib
import Parser

-- TODO: Load and store text
-- TODO: Bool for file chooser
-- TODO: Document everything
data EditorState n = EditorState {_text :: String, _index :: Int}

makeLenses ''EditorState

-- TODO: Display cursor above parsed output
drawUI :: EditorState () -> [T.Widget ()]
drawUI s = [ui]
  where
    (before, after) = splitAt (s ^. index) (s ^. text)
    ast = parseString (s ^. text)
    ui = 
      withAttr (A.attrName "highlight") (hCenter $ str "Syntax-Aware Editor")
          <=> strWrap (before ++ "_" ++ after)
          <=> case ast of
            Just expr -> exprToWidget expr
            Nothing -> str ""
    -- TODO: Status bar with parsing error

-- Recursive function to build the colored text widget
-- TODO: Fix parser output; should be (x -> y -> add (mult x x) y) 2 3 for initial state below
exprToWidget :: Expr -> T.Widget n
exprToWidget (Apply a) = applyToWidget a
exprToWidget (Lambda n e) = 
  str n
  <+> str " -> "
  <+> exprToWidget e

applyToWidget :: Apply -> T.Widget n
applyToWidget (Basic b) = basicToWidget b
applyToWidget (Apply' a b) = 
  applyToWidget a
  <+> basicToWidget b

basicToWidget :: Basic -> T.Widget n
basicToWidget (Integer i) = str (show i)
-- TODO: Add highlighting to name if at cursor position
basicToWidget (Name n) = str n
-- TODO: Add highlighting to brace if at cursor position
basicToWidget (Expr' e) = str "(" <+> exprToWidget e <+> str ")"
basicToWidget (Pairs ps) = str "{" <+> pairsToWidget ps <+> str "}"

pairToWidget :: Pair -> T.Widget n
pairToWidget (Pair n e) = 
  str n
  <+> str " = "
  <+> exprToWidget e

pairsToWidget :: [Pair] -> T.Widget n
pairsToWidget [] = str ""
pairsToWidget (p:ps) = pairToWidget p <+> (if null ps then str "" else str ", " <+> pairsToWidget ps)

currentWord :: EditorState () -> String
currentWord s =
  let text' = s ^. text
      index' = s ^. index
      (before, after) = splitAt index' text'
  in reverse (takeWhile (/= ' ') (reverse before)) ++ takeWhile (/= ' ') after

appEvent :: T.BrickEvent () e -> T.EventM () (EditorState ()) ()
appEvent (T.VtyEvent (V.EvKey e [])) = keyEvent e
appEvent _ = return ()

keyEvent :: V.Key -> T.EventM () (EditorState ()) ()
keyEvent (V.KChar c) = insertChar c
keyEvent V.KEnter = insertChar '\n'
keyEvent V.KBS = deleteChar
keyEvent V.KRight = incrementIndex
keyEvent V.KLeft = decrementIndex
keyEvent V.KEsc = M.halt
keyEvent _ = return ()

deleteChar :: T.EventM () (EditorState ()) ()
deleteChar = do
  cursor <- use index
  text %= \i -> delete' cursor i
  decrementIndex

insertChar :: Char -> T.EventM () (EditorState ()) ()
insertChar c = do
  cursor <- use index
  text %= \i -> insert' cursor c i
  index += 1

decrementIndex :: T.EventM () (EditorState ()) ()
decrementIndex = do
  index %= \i -> max 0 (i - 1)

incrementIndex :: T.EventM () (EditorState ()) ()
incrementIndex = do
  textLength <- use (text . to length)
  index %= \i -> min (i + 1) textLength

initialState :: EditorState ()
initialState = EditorState "(x -> y -> add (mult x x) y) 2 3" 0

theMap :: A.AttrMap
theMap = A.attrMap V.defAttr [(A.attrName "highlight", V.black `on` V.white)]

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
