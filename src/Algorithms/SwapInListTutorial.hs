module Algorithms.SwapInListTutorial
    ( swapByValue, swapByIndexOnChar, swapByIndexOnNum ) where

import Data.List (map)

swap :: (Num a, Eq a) => a -> a -> a -> a
swap x y z
    | z == x = y
    | z == y = x
    | otherwise = z

swapByValue :: (Num a, Eq a) => a -> a -> [a] -> [a]
swapByValue x y xs = map toSwap xs
    where toSwap = swap x y

folds :: Int -> Int -> Int -> a -> [a] -> [a]
folds _ _ _ _ [] = []
folds i x y temp (c:xs)
    | i == x = (xs!!(y-ii)):folds ii x y c xs
    | i == y = temp:folds ii x y temp xs
    | otherwise = c:folds ii x y temp xs
    where ii = (i+1)

swapByIndexOnChar :: Int -> Int -> [Char] -> [Char]
swapByIndexOnChar i j xs = folds 0 a b 'a' xs
    where
        a = min i j
        b = max i j

swapByIndexOnNum :: Num a => Int -> Int -> [a] -> [a]
swapByIndexOnNum i j xs = folds 0 a b 0 xs
    where
        a = min i j
        b = max i j



