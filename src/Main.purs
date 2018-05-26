module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, JArray, _Array, _Object, fromArray, jsonParser, jsonTrue)
import Data.Array (fromFoldable, some)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Lens (Traversal', _Just, preview, to, traversed, (^..))
import Data.Lens.At (at)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.String (fromCharArray)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Combinators (sepBy1)
import Text.Parsing.Parser.String (char, noneOf, string)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log (show (crawl "paras.thing.[].a" val) )

val :: Json
val = case jsonParser """{ "paras": { "thing": [{"a": 1}, {"a": 2}, {"a": 3}] } }""" of
           Left _ -> jsonTrue
           Right v -> v


followPath :: List Navigator -> Json -> Maybe Json
followPath (Key key : xs) v = do
  next <- preview (atKey key) v 
  followPath xs next
followPath (Traverse : xs) v = pure <<< toArray $ v ^.. traverseArray <<< to (followPath xs) <<< traversed
followPath Nil v = pure v

toArray :: forall f. Foldable f => f Json -> Json
toArray = fromArray <<< fromFoldable

atKey :: String -> Traversal' Json Json
atKey key = _Object <<< at key <<< _Just

traverseArray :: Traversal' Json Json
traverseArray = _Array <<< traversed

crawl :: String -> Json -> Maybe Json
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

parsePath :: String -> Maybe (List Navigator)
parsePath path = let result = runParser path pathP
                  in case result of
                          Left _ -> Nothing
                          Right res -> Just res


{-- val :: Foreign --}
{-- val  = toForeign { paras: [ { word: "Hello" }, { word: "World" } ] } --}
