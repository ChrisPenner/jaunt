module Main where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Argonaut (Json, _Array, _Object, encodeJson, jsonParser, jsonTrue)
import Data.Array (some)
import Data.Either (Either(..))
import Data.Lens (_Just, preview, to, traversed, (^?), (^..))
import Data.Lens.At (at)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..))
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
followPath (Key key : xs) v = (preview (_Object <<< at key <<< _Just) v) >>= followPath xs
followPath (Traverse : xs) v = Just <<< encodeJson $ v ^.. _Array <<< traversed <<< to (followPath xs) <<< _Just
followPath Nil v = Just v
{-- followPath _ _ = Nothing --}

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
