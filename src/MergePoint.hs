
module MergePoint ( LinkedList(..), createNode, push, last', reverse', init', head', mergePoint ) where

data LinkedList a = Empty | Node a (LinkedList a) deriving (Show, Eq, Ord)

createNode :: (Ord i, Eq i, Show i) => i -> (LinkedList i)
createNode val = Node val (Empty)


push :: LinkedList a -> LinkedList a -> LinkedList a
push (Node value _ ) list = (Node value list)

last' :: LinkedList a -> LinkedList a
last' Empty = Empty
last' (Node value Empty) = Node value Empty
last' (Node value list) = last' list

reverse' :: LinkedList a -> LinkedList a -> LinkedList a
reverse' (Node value list) Empty = reverse' list (Node value Empty)
reverse' Empty acc = acc
reverse' (Node value list) acc = reverse' list (Node value acc)

init' :: LinkedList a -> LinkedList a
init' (Node value Empty) = Empty
init' list = let (Node v rest) = reverse' list Empty in reverse' rest Empty

head' :: LinkedList a -> LinkedList a
head' Empty = Empty
head' (Node value _) = (Node value Empty)

compareItemsMergePoint :: (Ord a, Eq a, Show a) => LinkedList a -> LinkedList a -> LinkedList a -> LinkedList a
compareItemsMergePoint Empty l2 acc = acc
compareItemsMergePoint l1 Empty acc = acc
compareItemsMergePoint (Node v1 l1) (Node v2 l2) acc =
    if v1 == v2
        then compareItemsMergePoint l1 l2 (Node v1 Empty)
        else acc

mergePoint :: (Ord a, Eq a, Show a) =>  LinkedList a -> LinkedList a -> LinkedList a
mergePoint list1 list2 =
    let
        l1 = reverse' list1 Empty
        l2 = reverse' list2 Empty
        in
            (compareItemsMergePoint l1 l2 Empty)
