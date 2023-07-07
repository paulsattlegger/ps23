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
import Control.Applicative ((<|>))
import Control.Exception
import Control.Monad.IO.Class
import Data.Functor
import Data.Maybe (fromMaybe)
import Graphics.Vty qualified as V
import Lens.Micro.Platform
import Lib
import Parser
import System.Environment (getArgs)

data EditorState n = EditorState {_text :: String, _index :: Int, _path :: FilePath, _changed :: Bool}

makeLenses ''EditorState

-- | 'drawUI' is the main rendering function that creates the overall user interface.
-- It consists of two parts:
-- 1. A header that displays the current file path and an indicator if there are unsaved changes.
-- 2. The main text area of the editor which displays the contents of the file.
-- 3. A parser area, that validates the text and displays the result of the validation.
drawUI :: EditorState () -> [T.Widget ()]
drawUI s =
  [ drawHeader s <=> drawText s,
    drawParser s
  ]

-- | 'drawHeader' creates a header widget for the syntax-aware editor.
-- The header displays the current file path and an asterisk if there are unsaved changes.
drawHeader :: EditorState () -> T.Widget n
drawHeader s = withAttr (A.attrName "highlight") (hCenter $ str $ "Syntax-Aware Editor [" ++ s ^. path ++ (if s ^. changed then "*" else "") ++ "]")

-- | 'drawText' creates the main text widget for the syntax-aware editor.
-- It splits the text by new lines and applies highlighting to syntax elements.
drawText :: EditorState () -> T.Widget n
drawText s = drawLines (splitNewlines (highlightBrace (highlightBraceAtCursor (highlightNameAtCursor (currentWord before after) attrList) braceIndex)))
  where
    (before, after) = splitAt (s ^. index) (s ^. text)
    attrList = addDefaultAttr (splitAlphanumeric (before ++ "_" ++ after))
    braceIndex = indexOfBrace attrList before after

-- | 'drawParser' creates a parser widget for the syntax-aware editor.
-- This widget shows the parsing status of the text:
-- If the text is not valid according to the parser, it will display an error message.
-- If the text is valid, it will display a 'Valid' message.
drawParser :: EditorState () -> T.Widget n
drawParser s = padTop Max message
  where
    message = case parseString (s ^. text) of
      Left err -> withAttr (A.attrName "error") (strWrap err)
      Right _ -> withAttr (A.attrName "valid") (strWrap "Valid")

-- | Draws each line seperated by "\n" into a new vBox
drawLines :: [[(String, String, Int)]] -> T.Widget n
drawLines lines' = vBox (map listToWidget lines')

-- | Returns the index of the brace that the cursor currently points at.
-- Input: [(Attr, String-part, Index)] -> before -> after -> index
indexOfBrace :: [(String, String, Int)] -> String -> String -> Int
indexOfBrace list before _
  | null before = -1
  | last before == '(' = brace list "(" (countBraces before '(')
  | last before == ')' = brace list ")" (countBraces before ')')
  | last before == '{' = brace list "{" (countBraces before '{')
  | last before == '}' = brace list "}" (countBraces before '}')
  | otherwise = -1

-- | Returns the index of given string based on how often it already occured, i.e., get the index of the 3rd "(".
-- Input: list -> BraceType -> n-th occurance -> index
brace :: [(String, String, Int)] -> String -> Int -> Int
brace list s n = go list s n 0
  where
    go [] _ _ _ = error "String not found sufficiently often"
    go ((_, x, i) : xs) s' n' count
      | x == s' && count + 1 == n' = i
      | x == s' = go xs s' n' (count + 1)
      | otherwise = go xs s' n' count

-- | Returns the index of the matching brace if one exists.
-- Input: list -> index first brace -> list index second brace
matchingBrace :: [(String, String, Int)] -> Int -> Int
matchingBrace list i = fromMaybe (-1) match
  where
    pairs = parenPairs list ++ parenPairsCurly list
    match = lookup i pairs <|> lookup i (map swap pairs)
    swap (a, b) = (b, a)

-- | This function highlights unbalanced braces by checking for each brace if its index is contained in `parenPairs`.
highlightBrace :: [(String, String, Int)] -> [(String, String, Int)]
highlightBrace cs = map formatChar cs
  where
    formatChar (s, x, i)
      | x == "(" && not (isIndexInParenPairs i) = ("error", x, i)
      | x == ")" && not (isIndexInParenPairs i) = ("error", x, i)
      | x == "{" && not (isIndexInCurlyParenPairs i) = ("error", x, i)
      | x == "}" && not (isIndexInCurlyParenPairs i) = ("error", x, i)
      | otherwise = (s, x, i)

    isIndexInParenPairs i = i `elem` listToSet (parenPairs cs)
    isIndexInCurlyParenPairs i = i `elem` listToSet (parenPairsCurly cs)

-- | Given an index pair this function highlights both strings at the corresponding index by setting the attribute in the triple.
-- Input: Index pair -> list -> modified list
highlightBraceIndex :: (Int, Int) -> [(String, String, Int)] -> [(String, String, Int)]
highlightBraceIndex (a, b) = map format
  where
    format (s, x, i)
      | i == a = ("yellow", x, i)
      | i == b = ("yellow", x, i)
      | otherwise = (s, x, i)

-- | Highlights matching braces at the current cursor position.
-- Input: list -> list-Index -> modifed list
highlightBraceAtCursor :: [(String, String, Int)] -> Int -> [(String, String, Int)]
highlightBraceAtCursor list i
  | i == -1 = list
  | matchingBrace list i == -1 = list
  | otherwise = highlightBraceIndex (i, matchingBrace list i) list

-- | Highlights a given word in a list of colored words.
highlightNameAtCursor :: String -> [(String, String, Int)] -> [(String, String, Int)]
highlightNameAtCursor word = map format
  where
    format (s, x, i)
      | x == word = ("green", x, i)
      | otherwise = (s, x, i)

-- | Splits an list into seperate lines on a "\n"
splitNewlines :: [(String, String, Int)] -> [[(String, String, Int)]]
splitNewlines = go []
  where
    go acc [] = reverse $ map reverse acc
    go acc ((s, x, i) : xs)
      | x == "\n" = go ([("default", "\0", i)] : acc) xs
      | otherwise = case acc of
          [] -> go [[(s, x, i)]] xs
          (a : as) -> go (((s, x, i) : a) : as) xs

-- | Adds the "default" attribute to each entry of the input list as well as an increasing unique index
addDefaultAttr :: [String] -> [(String, String, Int)]
addDefaultAttr strings = zipWith (\s idx -> ("default", s, idx)) strings [0 ..]

-- | Converts the list into Brick widgets that can be displayed on the terminal
listToWidget :: [(String, String, Int)] -> T.Widget n
listToWidget [] = emptyWidget
listToWidget ((s, x, _) : cs)
  | s == "error" = withAttr (A.attrName "error") (str x) <+> listToWidget cs
  | s == "green" = withAttr (A.attrName "green") (str x) <+> listToWidget cs
  | s == "yellow" = withAttr (A.attrName "yellow") (str x) <+> listToWidget cs
  | otherwise = str x <+> listToWidget cs

-- | Returns a list of tuples that contains the indices of valid braces
-- From: https://stackoverflow.com/questions/10243290/determining-matching-parenthesis-in-haskell
-- Input: list -> [index pair of matching braces]
parenPairs :: [(String, String, Int)] -> [(Int, Int)]
parenPairs = go []
  where
    go _ [] = []
    go acc ((_, "(", i) : cs) = go (i : acc) cs
    go [] ((_, ")", _) : cs) = go [] cs -- unbalanced parentheses!
    go (i : is) ((_, ")", j) : cs) = (i, j) : go is cs
    go acc (_ : cs) = go acc cs

parenPairsCurly :: [(String, String, Int)] -> [(Int, Int)]
parenPairsCurly = go []
  where
    go _ [] = []
    go acc ((_, "{", i) : cs) = go (i : acc) cs
    go [] ((_, "}", _) : cs) = go [] cs -- unbalanced parentheses!
    go (i : is) ((_, "}", j) : cs) = (i, j) : go is cs
    go acc (_ : cs) = go acc cs

appEvent :: T.BrickEvent () e -> T.EventM () (EditorState ()) ()
appEvent (T.VtyEvent (V.EvKey e m)) = keyEvent e m
appEvent _ = return ()

-- | Handles key events for the editor.
keyEvent :: V.Key -> [V.Modifier] -> T.EventM () (EditorState ()) ()
keyEvent (V.KChar 'c') [V.MCtrl] = M.halt
keyEvent (V.KChar 's') [V.MCtrl] = dump
keyEvent (V.KChar c) _ = insertChar c
keyEvent V.KEnter _ = insertChar '\n'
keyEvent V.KBS _ = deleteChar
keyEvent V.KRight _ = incrementIndex
keyEvent V.KLeft _ = decrementIndex
keyEvent V.KEsc _ = M.halt
keyEvent _ _ = return ()

-- | Deletes the character at the current cursor position in the editor.
deleteChar :: T.EventM () (EditorState ()) ()
deleteChar = do
  cursor <- use index
  text %= \i -> delete' cursor i
  decrementIndex
  changed .= True

-- | Inserts the provided character at the current cursor position in the editor.
insertChar :: Char -> T.EventM () (EditorState ()) ()
insertChar c = do
  cursor <- use index
  text %= \i -> insert' cursor c i
  index += 1
  changed .= True

-- | Decrements the cursor index by one.
decrementIndex :: T.EventM () (EditorState ()) ()
decrementIndex = do
  index %= \i -> max 0 (i - 1)

-- | Increments the cursor index by one.
incrementIndex :: T.EventM () (EditorState ()) ()
incrementIndex = do
  textLength <- use (text . to length)
  index %= \i -> min (i + 1) textLength

-- | Saves the current state of the editor to a file.
dump :: T.EventM () (EditorState ()) ()
dump = do
  path' <- use path
  text' <- use text
  liftIO $ writeFile path' text'
  changed .= False

-- | Creates the initial state of the editor.
initialState :: String -> FilePath -> Bool -> EditorState ()
initialState text' = EditorState text' (length text')

-- | Defines the attribute map using the Brick library
-- for defining the visual style of the user interface.
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

-- | Defines the Brick application (named "editor"),
-- using the application constructor from the Brick library.
editor :: M.App (EditorState ()) e ()
editor =
  M.App
    { M.appDraw = drawUI,
      M.appChooseCursor = M.showFirstCursor,
      M.appHandleEvent = appEvent,
      M.appStartEvent = return (),
      M.appAttrMap = const theMap
    }

-- | Main entry point of a Haskell program.
main :: IO ()
main = do
  args <- getArgs
  case args of
    [path'] -> do
      either' <- try (readFile path') :: IO (Either IOException String)
      let (text', changed') = case either' of
            Left _ -> ("", True)
            Right t -> (t, False)
      void $ M.defaultMain editor (initialState text' path' changed')
    _ -> putStrLn "Usage: a3-exe <path>"
