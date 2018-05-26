module Parse where

import Prelude

import Control.Alt ((<|>))
import Data.Array (some)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (fromString) as Int
import Data.List (List)
import Data.Maybe (Maybe(Nothing, Just))
import Data.String (fromCharArray)
import Navigate (Navigator(..))
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, try) as P
import Text.Parsing.Parser.String (char, noneOf, string)




keyP :: Parser String Navigator
keyP = Key <<< fromCharArray <$> some (noneOf ['.', '[', ']'])

traverserP :: Parser String Navigator
traverserP = string "[]" $> Traverse

indexerP :: Parser String Navigator
indexerP = do
  digits <- P.between (char '[') (char ']') (fromCharArray <$> some (noneOf [']']))
  case Int.fromString digits of
       Just n -> pure $ Index n
       Nothing -> fail $ "expected number between [], got: " <> digits

pathP :: Parser String (List Navigator)
pathP = P.sepBy1 (P.try traverserP <|> indexerP <|> keyP) (char '.')

parsePath :: String -> Either String (List Navigator)
parsePath path = lmap parseErrorMessage $ runParser path pathP
