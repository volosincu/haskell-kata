
module MergePointSpec (nodeSpec, listSpec, mergePointSpec) where

import MergePoint (createNode, mergePoint, push, last', reverse', init', head', LinkedList(..))
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

            (Node d _) = node
            in
                d `shouldBe` value

    it "create single node" $ do
        let value :: Char
            value = 'a'

            node :: LinkedList Char
            node = (createNode value)

            (Node d empty) = node
	    in
	        empty `shouldBe` Empty

listSpec :: Spec
listSpec = describe "Spec list structure" $ do
    it "push node" $ do
        let value :: Int
            value = 7

            node :: LinkedList Int
            node = (createNode value)

            list :: LinkedList Int
            list = (createNode 2)
            newList = (push node list)
	    in
	        newList `shouldBe` (Node 7 (Node 2 (Empty)))

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
            actual = init' list
	    in
	        actual `shouldBe` expected

    it "get head of list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = head' list
	    in
	        actual `shouldBe` expected

    it "get head of single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = head' list
	    in
	        actual `shouldBe` expected

    it "get last of list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = createNode 'd'

            actual :: LinkedList Char
            actual = last' list
	    in
	        actual `shouldBe` expected

    it "get head of single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = createNode 'a'

            actual :: LinkedList Char
            actual = last' list
	    in
	        actual `shouldBe` expected

    it "reverse single element list" $ do
        let list :: LinkedList Char
            list = getList ['a']

            expected :: LinkedList Char
            expected = getList ['a']

            actual :: LinkedList Char
            actual = reverse' list Empty
	    in
	        actual `shouldBe` expected

    it "reverse list" $ do
        let list :: LinkedList Char
            list = getList ['a', 'b', 'c', 'd']

            expected :: LinkedList Char
            expected = getList ['d', 'c', 'b', 'a']

            actual :: LinkedList Char
            actual = reverse' list Empty
	    in
	        actual `shouldBe` expected

mergePointSpec :: Spec
mergePointSpec = describe "Spec merge point between 2 lists - There are only 3 posible scenarios - the lists can be totally different, identical or start different and merge at some point" $ do
    describe "Scenario 1" $ do
	it "search merge point in totaly different lists" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b']

                list1 :: LinkedList Char
		list1 = getList ['x', 'y', 'z']

		expected :: LinkedList Char
		expected = Empty

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

	it "search merge point in totaly different lists (first empty)" $ do
	    let list :: LinkedList Char
		list = getList []

                list1 :: LinkedList Char
		list1 = getList ['x', 'y', 'z']

		expected :: LinkedList Char
		expected = Empty

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

	it "search merge point in totaly different lists (second empty)" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b']

                list1 :: LinkedList Char
		list1 = getList []

		expected :: LinkedList Char
		expected = Empty

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

    describe "Scenario 2" $ do
    	it "search merge point in identic lists (single element lists)" $ do
	    let list :: LinkedList Char
		list = getList ['a']

                list1 :: LinkedList Char
		list1 = getList ['a']

		expected :: LinkedList Char
		expected = createNode 'a'

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

        it "search merge point in identic lists" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b']

                list1 :: LinkedList Char
		list1 = getList ['a', 'b']

		expected :: LinkedList Char
		expected = createNode 'a'

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

    	it "search merge point in identic lists" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b', 'c']

                list1 :: LinkedList Char
		list1 = getList ['a', 'b', 'c']

		expected :: LinkedList Char
		expected = createNode 'a'

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

    describe "Scenario 3" $ do
    	it "search merge point in unequal lists (first shorter)" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b', 'c', 'd']

                list1 :: LinkedList Char
		list1 = getList ['x', 'y', 'z', 'c', 'd']

		expected :: LinkedList Char
		expected = createNode 'c'

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

    	it "search merge point in unequal lists (diff equal list)" $ do
	    let list :: LinkedList Char
		list = getList ['x', 'y', 'z', 'c', 'd']

                list1 :: LinkedList Char
		list1 = getList ['r', 'w', 'q', 'c', 'd']

		expected :: LinkedList Char
		expected = createNode 'c'

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
		    actual `shouldBe` expected

    	it "search merge point in unequal lists (second shorter)" $ do
	    let list :: LinkedList Char
		list = getList ['a', 'b', 'x', 'y', 'z']

                list1 :: LinkedList Char
		list1 = getList ['x', 'y', 'z']

		expected :: LinkedList Char
		expected = (Node 'x' Empty)

		actual :: LinkedList Char
		actual = mergePoint list list1
                in
                    actual `shouldBe` expected

    	it "search merge point in unequal lists (equals)" $ do
	    let list :: LinkedList Char
		list = getList ['x', 'y', 'z']

                list1 :: LinkedList Char
		list1 = getList ['x', 'y', 'z']

		expected :: LinkedList Char
		expected = (Node 'x' Empty)

		actual :: LinkedList Char
		actual = mergePoint list list1
		in
                    actual `shouldBe` expected

