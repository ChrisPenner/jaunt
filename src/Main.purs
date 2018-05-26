module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, _Array, _Object, fromArray, jsonParser, jsonTrue)
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Int (fromString) as Int
import Data.Lens (Traversal', _Just, preview, to, traversed, (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.String (fromCharArray)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, try)
import Text.Parsing.Parser.String (anyChar, char, noneOf, string)
import Text.Parsing.Parser.Token (digit)
import Traverse

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show (crawl "paras.thing.[].a" val) )

val :: Json
val = case jsonParser """{ "paras": { "thing": [{"a": 1}, {"a": 2}, {"a": 3}] } }""" of
           Left _ -> jsonTrue
           Right v -> v


