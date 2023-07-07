module Lib (insert', delete', listToSet, currentWord, splitAlphanumeric, countBraces) where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (nub)

init' :: [a] -> [a]
init' [] = []
init' cs = init cs

insert' :: Int -> a -> [a] -> [a]
insert' i c cs =
  let (before, after) = splitAt i cs
   in before ++ [c] ++ after

delete' :: Int -> [a] -> [a]
delete' i cs =
  let (before, after) = splitAt i cs
   in init' before ++ after

-- | Converts list of index tuples to a set without duplicates.
listToSet :: [(Int, Int)] -> [Int]
listToSet pairs = nub (concatMap (\(start, end) -> [start, end]) pairs)

-- | 'currentWord' takes two strings, 'before' and 'after' (the cursor) and returns the word.
currentWord :: String -> String -> String
currentWord before after = reverse (takeWhile isAlpha (reverse before)) ++ takeWhile isAlpha after

-- | 'splitAlphanumeric' takes a string and breaks it into a list of alphanumeric words.
splitAlphanumeric :: String -> [String]
splitAlphanumeric [] = []
splitAlphanumeric (x : xs)
  | isAlphaNum x =
      let (alphanum, rest) = span isAlphaNum (x : xs)
       in alphanum : splitAlphanumeric rest
  | otherwise = [x] : splitAlphanumeric xs

-- | Counts how often the given brace type occours in the String.
-- Input: Input-String -> BraceType -> Count
countBraces :: String -> Char -> Int
countBraces xs x = foldl (\count char -> if char == x then count + 1 else count) 0 xs