module Test.Main where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff, kind Effect)
import Data.Argonaut (class EncodeJson, Json, encodeJson, fromArray, fromNumber, fromString, jsonParser)
import Data.Either (Either)
import Data.List (fromFoldable) as L
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (RunnerEffects, run)
import Traverse (crawl)

exampleJson :: Either String Json
exampleJson = jsonParser $ """
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

testJson :: forall a. EncodeJson a => String -> Array a -> _
testJson path expected =
  let actual = exampleJson >>= crawl path
   in actual `shouldEqual` pure (encodeJson <$> L.fromFoldable expected)

main :: Eff (RunnerEffects ()) Unit
main = run [consoleReporter] $ do
  describe "crawl" do

    describe "handles single paths" do
      it "finds strings" $ testJson "str" ["Hello"]
      it "finds numbers" $ testJson "num" [123]
      it "finds arrays" $ testJson "arr" [[1, 2, 3]]
      it "finds objects" $ testJson "obj" [(fromFoldable [Tuple "a" 1])]

    describe "dives into objects" do
      it "finds properties with ." $ testJson "obj.a" [1]
      it "finds nested properties with . chains" $ testJson "nested.deep.val" [42]

    describe "traverses arrays" do
      it "finds each element with []" $ testJson "arr.[]" [1, 2, 3]
      it "finds each nested element with []" $ testJson "nestedArr.[].val" [1, 2, 3]
      it "finds indexes with .[i]" $ testJson "arr.[0]" [1]

