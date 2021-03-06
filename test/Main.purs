module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (class EncodeJson, Json, encodeJson, fromArray, fromNumber, fromString, jsonParser)
import Data.Either (Either(..))
import Data.List (fromFoldable) as L
import Data.Maybe (Maybe(..))
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Navigate (Builder(..), Navigator(..), Path)
import Parse (parseExpr)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Traverse (crawl, runJaunt)

exampleJson :: String
exampleJson = """
{
  "str": "Hello",
  "num": 123,
  "null": null,
  "arr": [1, 2, 3],
  "obj": {
    "a": 1
  },
  "nested": {
      "deep": {
          "val": 42
        }
    },
  "nestedArr": [
      {
        "val": 1
      },
      {
        "val": 2
      },
      {
        "val": 3
      }
    ]
}
"""

testParse :: String -> Builder Path -> _
testParse str expected = parseExpr str `shouldEqual` Right expected

testJson :: forall a. EncodeJson a => String -> Array a -> _
testJson = testSpecificJson exampleJson

testErrs :: String -> String -> String -> _
testErrs jsonString path expectedErr =
  let actual = jsonParser jsonString >>= runJaunt <<< crawl path
   in actual `shouldEqual` Left expectedErr

testSpecificJson :: forall a. EncodeJson a => String -> String -> Array a -> _
testSpecificJson jsonString path expected =
  let actual = jsonParser jsonString >>= runJaunt <<< crawl path
   in actual `shouldEqual` pure (encodeJson <$> L.fromFoldable expected)


main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  describe "parser" do

    describe "indexer" do
      it "should parse slices" $ (testParse ".[1:2]") (BPipes <<< pure <<<  BVal <<< pure $ Slice (Just 1) (Just 2))


  describe "crawl" do

    describe "handles single paths" do
      it "finds strings" $ testJson ".str" ["Hello"]
      it "finds numbers" $ testJson ".num" [123]
      it "finds arrays" $ testJson ".arr" [[1, 2, 3]]
      it "finds objects" $ testJson ".obj" [(fromFoldable [Tuple "a" 1])]
      it "works with identity" $ testSpecificJson "1" "." [1]
      {-- it "obj index notation" $ testJson ".obj.[\"a\"]" [1] --}

    describe "dives into objects" do
      it "finds properties with ." $ testJson ".obj.a" [1]
      it "finds nested properties with . chains" $ testJson ".nested.deep.val" [42]

    {-- describe "traverses objects" do --}
      {-- it "finds each value with []" $ testSpecificJson "{\"a\": 1, \"b\": 2}" ".[]" [1, 2] --}

    {-- describe "parens" do --}
      {-- it "group expressions" $ testSpecificJson "1" "(. + 2) * 5" [1, 2] --}

    {-- describe "recursive descent with .." do --}
      {-- it "group expressions" $ testSpecificJson "1" "(. + 2) * 5" [1, 2] --}

    {-- describe "math" do --}
    {-- describe "length" do --}
    {-- describe "keys func" do --}



    describe "traverses arrays" do
      it "finds each element with []" $ testJson ".arr.[]" [1, 2, 3]
      it "finds each nested element with []" $ testJson ".nestedArr.[].val" [1, 2, 3]
      it "finds indexes with .[i]" $ testJson ".arr.[0]" [1]

      describe "errors" do
        it "should show errors" $ testErrs "{\"a\": [1, 2]}" ".a.[].missing" "couldn't find (missing) in 1"

    describe "pipes" do
      it "passes simple filters" $ testJson ".nested | .deep | .val" [42]
      it "passes complex filters" $ testJson ".nested | [ .deep ] | .[] | { a: { b: .val } } | .a.b " [42]

    describe "optional params" do
      it "should not error if missing AND optional" $ testJson ".missing?" ([] :: Array Json)
      it "should not error if missing AND optional AND chained" $ testSpecificJson "{}" ".a?.b?.c?" ([] :: Array Json)
      it "SHOULD error if missing and NOT optional" $ testErrs "{}" ".missing" "couldn't find (missing) in {}"
      {-- it "SHOULD error if indexing into non-array or object" $ testErrs "1" ".[]" "can only iterate over objects or arrays" --}

    describe "array slices" do
      it "should return slice from array" $ testJson ".arr.[1:2]" [[2, 3]]
      it "slices up till end if missing end index" $ testSpecificJson "[1, 2, 3, 4]" ".[2:]" [[3, 4]]
      it "slices from start if missing start index" $ testSpecificJson "[1, 2, 3, 4]" ".[:2]" [[1, 2, 3]]
      it "slices whole array if missing both start and end index" $ testSpecificJson "[1, 2, 3, 4]" ".[:]" [[1, 2, 3, 4]]

    {-- describe "multiple top-level filters separated by ','" do --}
      {-- it "should return slice from array" $ testJson ".str, .num" [fromString "Hello", fromNumber 123.0] --}


  describe "build" do
    describe "arrays" do
      it "using a path" $ testJson "[ .str ]" [["Hello"]]
      it "using nested paths" $ testJson "[ .nested.deep.val ]" [[42]]
      it "using multiple paths" $ testJson "[ .nested.deep.val, .obj.a ]" [fromArray <<< map fromNumber $ [42.0, 1.0 ]]

    describe "objects" do
      it "using a simple path" $ testJson "{ result: .str }" [(fromFoldable [Tuple "result" "Hello"])]
      it "using a nested path" $ testJson "{ result: .nested.deep.val }" [(fromFoldable [Tuple "result" 42])]
      it "using multiple keys" $ testJson "{ a: .str, b: .num }" [(fromFoldable [Tuple "a" (fromString "Hello"), Tuple "b" (fromNumber 123.0)])]
      it "using array expansion" $ testJson "{ a: .arr.[] }" (fromFoldable <$> [[Tuple "a" 1], [Tuple "a" 2], [Tuple "a" 3]])
      it "builds nested objects" $ testJson "{ a: { b: .str } }" [(fromFoldable [Tuple "a" (fromFoldable [Tuple "b" "Hello"])])]
      it "using array expansion in permutations" $ testJson "{ a: .arr.[], b: .arr.[] }" (fromFoldable <$> do
                                                             a <- [1, 2, 3]
                                                             b <- [1, 2, 3]
                                                             pure [Tuple "a" a, Tuple "b" b])


