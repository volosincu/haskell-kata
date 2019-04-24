{-# LANGUAGE QuasiQuotes #-}

module ParseDom (documentToStack, isValidStack) where

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
isValidStack :: [String] -> Bool
isValidStack stack = False