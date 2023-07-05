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
import Control.Applicative ((<|>))
import Data.Char
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib
import Parser
import Data.List (nub)

-- TODO: Load and store text
-- TODO: Bool for file chooser
-- TODO: Document everything
-- TODO: Also support "{}" braces
data EditorState n = EditorState {_text :: String, _index :: Int}

makeLenses ''EditorState

-- TODO: Display cursor above parsed output
drawUI :: EditorState () -> [T.Widget ()]
drawUI s = [ui, parser]
  where
    (before, after) = splitAt (s ^. index) (s ^. text)
    word = currentWord before after
    attrList = addDefaultAttr (splitAlphanumeric (before ++ "_" ++ after))
    braceIndex = getIndexOfBrace attrList before after
    message = case parseString (s ^. text) of
      Left err -> withAttr (A.attrName "error") (strWrap err)
      Right _ -> withAttr (A.attrName "valid") (strWrap "Valid")
    ui =
      withAttr (A.attrName "highlight") (hCenter $ str "Syntax-Aware Editor")
        <=> drawLines (splitNewlines (highlightBrace (highlightBraceAtCursor (highlightNameAtCursor word attrList) braceIndex)))
    parser = padTop Max message

getIndexOfBrace :: [(String, String, Int)] -> String -> String -> Int
getIndexOfBrace ast before after
  | null before = -1
  | last before == '(' = findBraceInAst ast "(" (countBraces before '(')
  | last before == ')' = findBraceInAst ast ")" (countBraces before ')')
  | otherwise = -1

countBraces :: String -> Char -> Int
countBraces xs x = foldl (\count char -> if char == x then count + 1 else count) 0 xs

findBraceInAst :: [(String, String, Int)] -> String -> Int -> Int
findBraceInAst ast str n = go ast str n 0
  where
    go [] _ _ _ = error "String not found enough times in AST"
    go ((_, x, i):xs) str n count
      | x == str && count + 1 == n = i
      | x == str = go xs str n (count + 1)
      | otherwise = go xs str n count

getMatchingBrace :: [(String, String, Int)] -> Int -> Int
getMatchingBrace ast i = maybe (error "No matching brace found") id match
  where
    pairs = parenPairs ast
    match = lookup i pairs <|> lookup i (map swap pairs)
    swap (a, b) = (b, a)

highlightBraceIndex :: (Int, Int) -> [(String, String, Int)] -> [(String, String, Int)]
highlightBraceIndex (a, b) = map format
  where
    format (s, x, i)
      | i == a = ("yellow", x, i)
      | i == b = ("yellow", x, i)
      | otherwise = (s, x, i)

highlightBraceAtCursor :: [(String, String, Int)] -> Int -> [(String, String, Int)]
highlightBraceAtCursor ast i 
  | i == -1 = ast 
  | otherwise = highlightBraceIndex (i, getMatchingBrace ast i) ast

-- Draws each line seperated by "\n" into a new vBox
drawLines :: [[(String, String, Int)]] -> T.Widget n
drawLines lines' = vBox (map astToWidget lines')

-- Splits an AST into seperate lines on a "\n"
splitNewlines :: [(String, String, Int)] -> [[(String, String, Int)]]
splitNewlines = go []
  where
    go acc [] = reverse $ map reverse acc
    go acc ((s, x, i):xs)
      | x == "\n" = go ([("default", " ", i)]:acc) xs
      | otherwise = case acc of
                      []     -> go [[(s, x, i)]] xs
                      (a:as) -> go (((s, x, i):a):as) xs

highlightNameAtCursor :: String -> [(String, String, Int)] -> [(String, String, Int)]
highlightNameAtCursor word = map format
  where
    format (s, x, i)
      | x == word = ("green", x, i)
      | otherwise = (s, x, i)

-- Adds the "default" attribute to each entry of the input list as well as an increasing unique index
addDefaultAttr :: [String] -> [(String, String, Int)]
addDefaultAttr strings = zipWith (\str idx -> ("default", str, idx)) strings [0..]

astToWidget :: [(String, String, Int)] -> T.Widget n
astToWidget [] = emptyWidget
astToWidget ((s, x, _) : cs)
  | s == "error" = withAttr (A.attrName "error") (str x) <+> astToWidget cs
  | s == "green" = withAttr (A.attrName "green") (str x) <+> astToWidget cs
  | s == "yellow" = withAttr (A.attrName "yellow") (str x) <+> astToWidget cs
  | otherwise = str x <+> astToWidget cs

splitAlphanumeric :: String -> [String]
splitAlphanumeric [] = []
splitAlphanumeric (x : xs)
  | isAlphaNum x =
      let (alphanum, rest) = span isAlphaNum (x : xs)
       in alphanum : splitAlphanumeric rest
  | otherwise = [x] : splitAlphanumeric xs

-- Input: [Attr, String, Index]
-- Returns a list of tuples that contains the indices of valid braces
-- From: https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell
parenPairs :: [(String, String, Int)] -> [(Int, Int)]
parenPairs = go []
  where
    go _ [] = []
    go acc ((_, "(", i) : cs) = go (i : acc) cs
    go [] ((_, ")", _) : cs) = go [] cs -- unbalanced parentheses!
    go (i : is) ((_, ")", j) : cs) = (i, j) : go is cs
    go acc (_ : cs) = go acc cs


-- Converts list of index tuples to a set (without duplicates)
listToSet :: [(Int, Int)] -> [Int]
listToSet pairs = nub (concatMap (\(start, end) -> [start, end]) pairs)

-- Checks if an index is contained in the list
indexInList :: Int -> [Int] -> Bool
indexInList index indices = index `elem` indices


highlightBrace :: [(String, String, Int)] -> [(String, String, Int)]
highlightBrace cs = map formatChar cs
  where
    formatChar (s, x, i)
      | x == "(" && not (isIndexInParenPairs i) = ("error", x, i)
      | x == ")" && not (isIndexInParenPairs i) = ("error", x, i)
      | otherwise = (s, x, i)

    isIndexInParenPairs i = indexInList i (listToSet (parenPairs cs))

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
      (A.attrName "yellow", V.black `on` V.yellow),
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
