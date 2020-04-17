module HackerRank.ChaosQueue ( minimumBribes ) where

import Control.Monad
import Data.Tuple (fst, snd)
import Data.List as List
import Data.Map as Map
import Debug.Trace

diffLoc :: Num t => (t -> [a] -> [a]) -> t -> [t] -> [a]
diffLoc f i [] = []
diffLoc f i (x:xs) = (-1*(i-x))`f`diffLoc f (i+1) xs

swapAndUpdateValues :: (Eq b, Num b) 
    => b -> (b, b) -> b -> (b, b) -> (b, b) -> (b, b)
swapAndUpdateValues maxIndex (k, v) pairIndex (k1, v1) c
    | c == (k1, v1) = (k, nextValueOfMax)
    | c == (k, v) = (k1, nextValueOfPair)
    | otherwise = c
    where
        i = maxIndex+1
        j = pairIndex+1
        nextValueOfMax = (-1*(j-k))
        nextValueOfPair = (-1*(i-k1))

calcStep :: (Ord k, Show k, Num a) => k -> k -> Map k a -> Map k a
calcStep max pair state =
    if (max < pair)
        then trace ("update pair " ++ show max ++ "<" ++ show pair) Map.insertWith (+) pair 1 state
        else trace ("update max " ++ show max ++ ">=" ++ show pair) Map.insertWith (+) max 1 state

reverseSteps :: (Show a, Show t, Num t, Num a, Ord a)
    => [(Int, Int)] -> Map Int a -> t -> Maybe t
reverseSteps kvs itemStepsMap step =
    if 0 == v
        then trace ("Nu mai sunt maxime " ++ show maxStepsPerElem) Just step
        else if maxStepsPerElem >=3
                then trace ("s-a atins nr maxim de spagi " ++ show sw) Nothing
                else trace ("rec reverseSteps " ++ show maxStepsPerElem ++ " " ++ show step ++ "  " ++ show allMaxims ++ "  " ++ show sw  ) reverseSteps sw newItemStepsMap (step+1)
    where
        randomMax = getMax kvs
        (kz, vz) = randomMax
        allMaxims = getMaxs kvs randomMax
        max = validateMax kvs allMaxims
        (k, v) = max
        Just keyIndex = List.findIndex (== max) kvs
        nextIndex = if v<0 then keyIndex-1 else keyIndex+1 
        pair = kvs!!nextIndex
        (k1, v1) = pair
        newItemStepsMap = calcStep k k1 itemStepsMap
        maxStepsPerElem = (maximum . Map.elems) newItemStepsMap
        toSwap = swapAndUpdateValues keyIndex max nextIndex pair
        sw = List.map toSwap kvs

getMax :: (Ord a1, Num a1) => [(a2, a1)] -> (a2, a1)
getMax kvs = max
    where
        max = List.foldr
                (\v acc ->
                    if abs (snd v) > abs (snd acc)
                    then v
                    else acc)
                init
                kvs
        init = kvs!!0

getMaxs :: (Eq b, Num b) => [(a1, b)] -> (a2, b) -> [(a1, b)]
getMaxs kvs max = maxs
    where
        (k, v) = max
        maxs = List.filter (\e -> (abs $ snd e) == abs v) kvs

validateMax :: (Show a, Eq a, Ord a, Num a) => [(a,a)] -> [(a,a)] -> (a,a)
validateMax kvs (max:[]) = max
validateMax kvs (max:ms) =
    if v == 0 
        then max
        else if k < k1 && v == v1
            then trace("    _____     validate " ++ show k ++ "  " ++ show k1 ) validateMax kvs ms
            else max
    where
        (k, v) = max
        Just keyIndex = List.findIndex (== max) kvs
        nextIndex = if v<0 then keyIndex-1 else keyIndex+1 
        pair = kvs!!nextIndex
        (k1, v1) = pair

minimumBribes :: Monad m => [Int] -> m String
minimumBribes q =
    case steps of
        (Just counter) -> trace ("result " ++ show counter) return (show counter)
        Nothing -> trace ("result Too chaotic") return "Too chaotic"
    where
        steps = trace ("rec reverseSteps init" ++ show 0 ++ " " ++ show map ) reverseSteps map Map.empty 0
        ps = diffLoc (:) 1 q
        map = List.zip q ps