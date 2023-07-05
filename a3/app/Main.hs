{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Brick.AttrMap qualified as A
import Brick.Main qualified as M
import Brick.Types qualified as T
import Brick.Util (fg, on)
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Core (Padding (Max), emptyWidget, padTop, str, strWrap, vBox, withAttr, (<+>), (<=>))
import Control.Monad (void)
import Data.Char
import Data.List.Split
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib
import Parser
import Data.List (nub)

-- TODO: Load and store text
-- TODO: Bool for file chooser
-- TODO: Document everything
data EditorState n = EditorState {_text :: String, _index :: Int}

makeLenses ''EditorState

-- TODO: Display cursor above parsed output
drawUI :: EditorState () -> [T.Widget ()]
drawUI s = [ui, parser]
  where
    (before, after) = splitAt (s ^. index) (s ^. text)
    word = currentWord before after
    message = case parseString (s ^. text) of
      Left err -> withAttr (A.attrName "error") (strWrap err)
      Right _ -> withAttr (A.attrName "valid") (strWrap "Valid")
    ui =
      withAttr (A.attrName "highlight") (hCenter $ str "Syntax-Aware Editor")
        <=> drawLines word (splitOn "\n" $ before ++ "_" ++ after)
        <=> highlightBrace (before ++ "_" ++ after)
    parser = padTop Max message

--   Due to a limitation of the 'str' widget, which does not render empty strings,
--   lines that are empty are replaced with a single space
--   as a workaround to ensure they are still drawn as blank lines.
drawLines :: String -> [String] -> T.Widget n
drawLines word lines' = vBox (map toVBox lines')
  where
    toVBox line = if line == "" then str " " else drawLine word line

drawLine :: String -> String -> T.Widget n
drawLine word line = foldr ((<+>) . format) emptyWidget (splitAlphanumeric line)
  where
    format x
      | x == word = withAttr (A.attrName "green") (str x)
      | otherwise = str x

splitAlphanumeric :: String -> [String]
splitAlphanumeric [] = []
splitAlphanumeric (x : xs)
  | isAlphaNum x =
      let (alphanum, rest) = span isAlphaNum (x : xs)
       in alphanum : splitAlphanumeric rest
  | otherwise = [x] : splitAlphanumeric xs

-- Returns a list of tuples that contains the indices of valid braces
-- From: https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell
parenPairs :: String -> [(Int, Int)]
parenPairs = go 0 []
  where
    go _ _        []         = []
    go j acc      ('(' : cs) =          go (j + 1) (j : acc) cs
    go j []       (')' : cs) =          go (j + 1) []        cs -- unbalanced parentheses!
    go j (i : is) (')' : cs) = (i, j) : go (j + 1) is        cs
    go j acc      (c   : cs) =          go (j + 1) acc       cs

-- Converts list of index tuples to a set (without duplicates)
listToSet :: [(Int, Int)] -> [Int]
listToSet pairs = nub (concatMap (\(start, end) -> [start, end]) pairs)

-- Checks if an index is contained in the list
indexInList :: Int -> [Int] -> Bool
indexInList index indices = index `elem` indices

highlightBrace :: String -> T.Widget n
highlightBrace s = combineWidgets (zipWith (curry formatChar) s [0..])
  where
    formatChar (x, i)
      | x == '(' && not (isIndexInParenPairs i) = withAttr (A.attrName "error") (str [x])
      | x == ')' && not (isIndexInParenPairs i) = withAttr (A.attrName "error") (str [x])
      | otherwise = str [x]

    isIndexInParenPairs i = indexInList i (listToSet (parenPairs s))

    combineWidgets []     = emptyWidget
    combineWidgets (w:ws) = w <+> combineWidgets ws

currentWord :: String -> String -> String
currentWord before after = reverse (takeWhile isAlpha (reverse before)) ++ takeWhile isAlpha after

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
theMap =
  A.attrMap
    V.defAttr
    [ (A.attrName "highlight", V.black `on` V.white),
      (A.attrName "green", fg V.green),
      (A.attrName "valid", V.black `on` V.green),
      (A.attrName "error", V.white `on` V.red)
    ]

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
