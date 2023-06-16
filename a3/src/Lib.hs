module Lib (insert', delete') where

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