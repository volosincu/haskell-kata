{-# LANGUAGE QuasiQuotes #-}

module ParseDom (documentToStack, isValidStack, maxDepth) where

import qualified Data.List as L


-- |The 'documentToStack' function reads the document and builds a stack with the tags ignoring the tags content.
documentToStack :: String -> [String] -> [String]
documentToStack [] stack = L.reverse stack
documentToStack (x:xs) stack =
    if x == '<' then
       let tag = takeWhile matchTagEnd xs
           restDom = drop (length tag) xs
       in documentToStack restDom $ [tag] ++ stack
    else
        let dom = dropWhile matchTagStart xs
        in documentToStack dom stack
    where
        matchTagStart = (/= '<')
        matchTagEnd = (/= '>')

-- |The 'isValidStack' function reads the stack while the tags match. If all match the function returns 'True'
isValidStack :: [String] -> [String] -> Bool
isValidStack [] [] = True
isValidStack [] stack = False
isValidStack (tag:tags) []
    | isClosing = False
    | otherwise = isValidStack tags [tag]
    where isClosing = head tag == '/'
isValidStack (tag:tags) (stackTag:stack)
    | isClosing && isMatching = isValidStack tags stack
    | isClosing && not isMatching = isValidStack [] (tag:stack)
    | otherwise = isValidStack tags (tag:stackTag:stack)
    where
        isClosing = head tag == '/'
        isMatching = tail tag == stackTag

-- |The 'maxDepth' iterate tags from stack and compute the max depth 
maxDepth :: [String] -> Int -> Int -> Int
maxDepth [] current max = max
maxDepth tags current max = 0
maxDepth tags current max = 0 