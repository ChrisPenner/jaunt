module Traverse where

import Prelude

import Control.Monad.Except (class MonadError, ExceptT(..), except, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.MonadPlus (empty)
import Data.Argonaut (Json, _Array, _Object, fromArray, fromObject)
import Data.Array (fromFoldable)
import Data.Either (Either(..), either)
import Data.Foldable (class Foldable)
import Data.Lens (Traversal', _Just, _Right, elementsOf, preview, to, traversed, (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.List (List(..), drop, take, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Navigate (Builder(..), Navigator(..), Path)
import Parse (parseExpr)

type JauntM a = ExceptT String List a

runJaunt :: forall a. JauntM a -> Either String (List a)
runJaunt = sequence <<< runExceptT

followPath :: List Navigator -> Json -> JauntM Json
followPath (Key key isOptional : xs) v = 
  let err :: JauntM Json
      err = if isOptional then lift Nil
                          else throwError ("couldn't find (" <> key <> ") in " <> show v)
      result :: Maybe Json
      result = preview (atKey key) v
   in (err `maybe` followPath xs) result
followPath (Traverse : xs) v = do
  val <- lift $ v ^.. _Array <<< traversed
  followPath xs val
followPath (Index n : xs) v = do
  case v ^? _Array <<< ix n of
       Nothing -> throwError $ "index " <> show n <> " is out of bounds"
       Just v' -> followPath xs v'
followPath (Slice start end : xs) v =
  let elems = getSlice start end $ v ^.. _Array <<< traversed
   in followPath xs (toJsonArray elems)
followPath Nil v = pure v

getSlice :: forall a. Maybe Int -> Maybe Int -> List a -> List a
getSlice (Just start) (Just end) = take ((end - start) + 1) <<< drop start
getSlice Nothing (Just end) = take (end + 1)
getSlice (Just start) Nothing = drop start
getSlice Nothing Nothing = id

toJsonArray :: List Json -> Json
toJsonArray = fromArray <<< fromFoldable

atKey :: String -> Traversal' Json Json
atKey key = _Object <<< at key <<< _Just

traverseArray :: Traversal' Json Json
traverseArray = _Array <<< traversed

joinResults :: forall a f m. Functor f => Monad m => f (m (m a)) -> f (m a)
joinResults = map join

runBuilder :: Json -> Builder Path -> JauntM Json
runBuilder json (BVal path) = followPath path json
runBuilder json (BList paths) =
  toJsonArray <$> traverse (runBuilder json) paths
runBuilder json (BObject obj) = fromObject <$> traverse (runBuilder json) obj
runBuilder json (BPipes (expr : rest)) = do
  result <- runBuilder json expr
  runBuilder result (BPipes rest)
runBuilder json (BPipes Nil) = pure json

crawl :: String -> Json -> JauntM Json
crawl path json = either throwError (runBuilder json) (parseExpr path)
