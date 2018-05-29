module Main where

import Prelude
import Traverse

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION, throw)
import Data.Argonaut (Json, _Array, _Object, fromArray, jsonParser, jsonTrue, stringify)
import Data.Array (fromFoldable, some)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, intercalate)
import Data.Int (fromString) as Int
import Data.Lens (Traversal', _Just, preview, to, traversed, (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.List (List(Nil), (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.String (fromCharArray)
import Node.Buffer (BUFFER)
import Node.Buffer (toString) as B
import Node.Encoding (Encoding(..))
import Node.Process (PROCESS, argv, exit, stdin)
import Node.Stream (read)
import Node.Yargs.Applicative (Y, runY, yarg)
import Node.Yargs.Setup (YargsSetup)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, try)
import Text.Parsing.Parser.String (anyChar, char, noneOf, string)
import Text.Parsing.Parser.Token (digit)

queryString :: Y String
queryString = yarg "query" ["q"] Nothing (Right "query string is required") true

yargsSetup :: YargsSetup
yargsSetup = mempty

main :: forall e. Eff (_) Unit
main = runY yargsSetup (app <$> queryString)

app :: String -> Eff _ Unit
app queryString = do
  mJsonBuffer <- read stdin Nothing
  jsonText <- (maybe (throw "no stdin") pure mJsonBuffer) >>= B.toString UTF8
  let res = do
        json <- jsonParser jsonText
        results <- runJaunt $ crawl queryString json
        pure $ intercalate "\n" (stringify <$> results)
  pure unit

val :: Json
val = case jsonParser """{ "paras": { "thing": [{"a": 1}, {"a": 2}, {"a": 3}] } }""" of
           Left _ -> jsonTrue
           Right v -> v


