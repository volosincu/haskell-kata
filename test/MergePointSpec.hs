
module MergePointSpec (nodeSpec, listSpec) where

import MergePoint (createNode, push, last', reverse', init', head', LinkedList(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit


getList :: [Char] -> LinkedList Char
getList [] = Empty
getList (c:cs) = push (createNode c) (getList cs)


nodeSpec :: Spec
nodeSpec = describe "Spec node structure" $ do
    it "create node and verify value" $ do
        let value :: Int
            value = 7

            node :: LinkedList Int
            node = (createNode value)

            (Node d _) = node in d `shouldBe` value

    it "create single node" $ do
        let value :: Char
            value = 'a'

            node :: LinkedList Char
            node = (createNode value)

            (Node d empty) = node in empty `shouldBe` Empty

listSpec :: Spec
listSpec = describe "Spec list structure" $ do
    it "push node" $ do
        let value :: Int
            value = 7

            node :: LinkedList Int
            node = (createNode value)

            list :: LinkedList Int
            list = (createNode 2)
            newList = (push node list) in newList `shouldBe` (Node 7 (Node 2 (Empty)))

    it "get init of list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = getList ['a', 'b', 'c']

            actual :: LinkedList Char
            actual = init' list in actual `shouldBe` expected

    it "get init of single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = Empty

            actual :: LinkedList Char
            actual = init' list in actual `shouldBe` expected

    it "get head of list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = head' list in actual `shouldBe` expected

    it "get head of single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = head' list in actual `shouldBe` expected


    it "get last of list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = createNode 'd'

            actual :: LinkedList Char
            actual = last' list in actual `shouldBe` expected

    it "get head of single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = last' list in actual `shouldBe` expected