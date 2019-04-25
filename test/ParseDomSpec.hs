

module ParseDomSpec (
    maxDepthSpec
    , documentToStackSpec'PositiveCases
    , documentToStackSpec'EdgeCases
    , isValidStackSpec'ValidCase
    , isValidStackSpec'NotValidCase ) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.HUnit

import ParseDom (documentToStack, isValidStack, maxDepth)

documentToStackSpec'PositiveCases :: Spec
documentToStackSpec'PositiveCases = describe "read dom and extract tags in stack structure" $ do
    it "parse first node" $ do
        (documentToStack "<div>a test</div>" []) `shouldBe` ["div", "/div"]
    it "parse node 1" $ do
        (documentToStack "<div></div>" []) `shouldBe` ["div", "/div"]
    it "parse node single node with content" $ do
        (documentToStack "<div>a test</div>" []) `shouldBe` ["div", "/div"]
    it "parse node empty" $ do
        (documentToStack "" []) `shouldSatisfy` null
    it "parse node with children" $ do
        (documentToStack "<div><span>a</span> <span>b</span></div>" []) `shouldBe` ["div", "span", "/span", "span", "/span", "/div"]
    it "parse node siblings nested" $ do
        (documentToStack "<div><div>abc</div><div><span></span></div></div>" []) `shouldBe` ["div", "div", "/div", "div", "span", "/span", "/div", "/div"]

documentToStackSpec'EdgeCases :: Spec
documentToStackSpec'EdgeCases = describe "read inconsisted dom and extract valid tags in stack structure" $ do
    it "parse dom with one unclosed tag" $ do
        (documentToStack "<div>a test" []) `shouldBe` ["div"]
    it "parse dom with 2 different tags" $ do
        (documentToStack "<div></dv>" []) `shouldBe` ["div", "/dv"]
    it "parse dom with unmatched children tag" $ do
        (documentToStack "<div>a test<ul></div>" []) `shouldBe` ["div", "ul", "/div"]
    it "parse node with starting diamond tag" $ do
        (documentToStack "<></div>" []) `shouldBe` ["", "/div"]
    it "parse wrong tags" $ do
        (documentToStack "<div> /span>" []) `shouldBe` ["div"]
        (documentToStack "<div> > /span>" []) `shouldBe` ["div"]
        (documentToStack "<div>> /span>" []) `shouldBe` ["div"]
        (documentToStack "<div>> <> /span>" []) `shouldBe` ["div", ""]
        (documentToStack "<div <> /span>" []) `shouldBe` [""]

isValidStackSpec'ValidCase :: Spec
isValidStackSpec'ValidCase = describe "Validate correct stack structure" $ do
    it "validate stacks - matching tags" $ do
        (isValidStack ["div", "/div"] []) `shouldBe` True
        (isValidStack ["div", "span", "/span","/div"] []) `shouldBe` True
        (isValidStack ["div", "span", "/span","/div", "ul", "li", "/li", "/ul"] []) `shouldBe` True
        (isValidStack ["div", "span", "/span", "span", "/span", "/div"] []) `shouldBe` True
        (isValidStack ["div", "span", "/span", "span", "/span", "/div"] []) `shouldBe` True
        (isValidStack ["ul", "li", "/li", "/ul", "div", "/div", "p", "span" ,"/span" ,"/p"] []) `shouldBe` True
        (isValidStack ["ul", "li", "/li", "/ul", "div", "/div", "p", "span", "span", "/span" ,"/span" ,"/p"] []) `shouldBe` True

isValidStackSpec'NotValidCase :: Spec
isValidStackSpec'NotValidCase = describe "Validate incorrect stack structure" $ do
    it "validate stacks - unmatching tags" $ do
        (isValidStack ["span", "/div"] []) `shouldBe` False
        (isValidStack ["span", "/span","/div"] []) `shouldBe` False
        (isValidStack ["div", "span", "/span","/div", "ul", "/li", "/ul"] []) `shouldBe` False
        (isValidStack ["div", "span", "/span", "spn", "/span", "/iv"] []) `shouldBe` False
        (isValidStack ["ul", "li", "/li", "/ul", "div", "/div", "p", "span", "span" ,"/span" ,"/p"] []) `shouldBe` False
        (isValidStack ["ul", "li", "/li", "/ul", "div", "/div", "i", "span", "span", "/span" ,"/span" ,"/p"] []) `shouldBe` False

maxDepthSpec :: Spec
maxDepthSpec = describe "Validate max depth of dom tags transformed in a stack" $ do
    it "deepth of 0" $ do
        (maxDepth ["div", "/div"] 0 0) `shouldBe` 0
    it "deepth of 1" $ do
        (maxDepth ["div", "div", "/div", "/div"] 0 0) `shouldBe` 1
    it "deepth of 2" $ do
        (maxDepth ["div", "div", "div", "/div", "/div", "/div"] 0 0) `shouldBe` 2
    it "deepth of 2" $ do
        (maxDepth ["div", "div", "div", "/div", "/div", "/div", "ul", "li", "ul", "/ul", "/li", "li", "/li", "/ul"] 0 0) `shouldBe` 2