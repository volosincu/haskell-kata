
module MergePoint ( LinkedList(..), createNode, push ) where

data LinkedList a = Empty | Node a (LinkedList a) deriving (Show, Eq, Ord)

createNode :: (Ord i, Eq i, Show i) => i -> (LinkedList i)
createNode val = Node val (Empty)


push :: LinkedList a -> LinkedList a -> LinkedList a
push (Node value _ ) list = (Node value list)
