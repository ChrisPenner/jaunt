module Parse where

import Prelude

import Control.Alt ((<|>))
import Control.Lazy (fix)
import Data.Array (many, some)
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Int (fromString) as Int
import Data.List (List)
import Data.Maybe (Maybe(Nothing, Just))
import Data.StrMap (fromFoldable) as SM
import Data.String (fromCharArray)
import Data.Tuple (Tuple(..))
import Navigate (Builder(..), Navigator(..), Path)
import Text.Parsing.Parser (Parser, fail, parseErrorMessage, runParser)
import Text.Parsing.Parser.Combinators (between, sepBy1, try) as P
import Text.Parsing.Parser.Combinators (sepBy)
import Text.Parsing.Parser.String (char, noneOf, string, whiteSpace)

type ExprP = Parser String (Builder Path)

lchar :: Char -> Parser String Char
lchar = lexeme <<< char

ignoreChars :: Array Char
ignoreChars = [' ', '\t', '\n']

specialChars = ['.', '[', ']', '"', '\'', ',', ':']

keyP :: Parser String String
keyP = fromCharArray <$> lexeme (some (noneOf (specialChars <> ignoreChars)))

traverserP :: Parser String Navigator
traverserP = lexeme (string "[]") $> Traverse

inSquares :: forall a. Parser String a -> Parser String a
inSquares = lexeme <<< P.between (lchar '[') (lchar ']') <<< lexeme

inBraces :: forall a. Parser String a -> Parser String a
inBraces = lexeme <<< P.between (lchar '{') (lchar '}') <<< lexeme


indexerP :: Parser String Navigator
indexerP = do
  digits <- inSquares (fromCharArray <$> some (noneOf ([']'] <> ignoreChars)))
  case Int.fromString digits of
       Just n -> pure $ Index n
       Nothing -> fail $ "expected number between [], got: " <> digits

pathP :: ExprP
pathP = BVal <$> do
  _ <- lchar '.'
  sepBy (P.try traverserP <|> indexerP <|> (Key <$> keyP)) (lchar '.')

parseExpr :: String -> Either String (Builder Path)
parseExpr path = lmap parseErrorMessage $ runParser path exprP

listBuilderP :: ExprP
listBuilderP = BList <$> inSquares (pathP `sepBy` lchar ',')

keyValP :: ExprP -> Parser String (Tuple String (Builder Path))
keyValP exprP' = do
  key <- keyP
  _ <- lchar ':'
  val <- exprP'
  pure (Tuple key val)

objectBuilderP :: ExprP -> ExprP
objectBuilderP exprP' = BObject <<< SM.fromFoldable <$>
                        inBraces (keyValP exprP' `sepBy` lchar ',')

exprP :: ExprP
exprP = fix $ \p -> do
  filters <- (listBuilderP <|> objectBuilderP p <|> pathP) `sepBy` lchar '|'
  pure $ BPipes filters

lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* whiteSpace
