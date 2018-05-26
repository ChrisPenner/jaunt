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
import Data.Lens (Traversal', _Just, preview, to, traversed, (^..))
import Data.Lens.At (at)
import Data.List (List(Nil), (:))
import Data.Maybe (maybe)
import Data.String (fromCharArray)
import Text.Parsing.Parser (Parser, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (char, noneOf, string)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show (crawl "paras.thing.[].a" val) )

val :: Json
val = case jsonParser """{ "paras": { "thing": [{"a": 1}, {"a": 2}, {"a": 3}] } }""" of
           Left _ -> jsonTrue
           Right v -> v


followPath :: List Navigator -> Json -> Either String Json
followPath (Key key : xs) v = let err = Left ("couldn't find " <> key)
                                  result = preview (atKey key) v
                               in (err `maybe` followPath xs) result
followPath (Traverse : xs) v = pure <<< toArray $ v ^.. traverseArray <<< to (followPath xs) <<< traversed
followPath Nil v = pure v

toArray :: forall f. Foldable f => f Json -> Json
toArray = fromArray <<< fromFoldable

atKey :: String -> Traversal' Json Json
atKey key = _Object <<< at key <<< _Just

traverseArray :: Traversal' Json Json
traverseArray = _Array <<< traversed

crawl :: String -> Json -> Either String Json
crawl path json = do
  p <- parsePath path
  followPath p json

data Navigator = Key String | Traverse

keyP :: Parser String Navigator
keyP = Key <<< fromCharArray <$> some (noneOf ['.', '[', ']'])

traverserP :: Parser String Navigator
traverserP = string "[]" $> Traverse

pathP :: Parser String (List Navigator)
pathP = sepBy1 (traverserP <|> keyP) (char '.')

parsePath :: String -> Either String (List Navigator)
parsePath path = lmap parseErrorMessage $ runParser path pathP


{-- val :: Foreign --}
{-- val  = toForeign { paras: [ { word: "Hello" }, { word: "World" } ] } --}
