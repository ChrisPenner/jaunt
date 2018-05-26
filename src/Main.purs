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
followPath (Index n : xs) v = do
  case v ^? _Array <<< ix n of
       Nothing -> Left $ "index " <> show n <> " is out of bounds"
       Just v' -> followPath xs v'
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

data Navigator = Key String | Traverse | Index Int

keyP :: Parser String Navigator
keyP = Key <<< fromCharArray <$> some (noneOf ['.', '[', ']'])

traverserP :: Parser String Navigator
traverserP = string "[]" $> Traverse

indexerP :: Parser String Navigator
indexerP = do
  digits <- between (char '[') (char ']') (fromCharArray <$> some (noneOf [']']))
  case Int.fromString digits of
       Just n -> pure $ Index n
       Nothing -> fail $ "expected number between [], got: " <> digits

pathP :: Parser String (List Navigator)
pathP = sepBy1 (try traverserP <|> indexerP <|> keyP) (char '.')

parsePath :: String -> Either String (List Navigator)
parsePath path = lmap parseErrorMessage $ runParser path pathP
