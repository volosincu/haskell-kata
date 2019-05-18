
module MergePointSpec (nodeSpec, listSpec) where

import MergePoint (createNode, push, LinkedList(..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit


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