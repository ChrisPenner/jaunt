module Traverse where

import Prelude

import Data.Argonaut (Json, _Array, _Object, fromArray, fromObject)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Lens (Traversal', _Just, _Right, elementsOf, preview, to, traversed, (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.Lens.Indexed (positions)
import Data.List (List(..), drop, take, (:))
import Data.Maybe (Maybe(..), maybe)
import Data.Traversable (sequence, traverse)
import Navigate (Builder(..), Navigator(..), Path)
import Parse (parseExpr)

followPath :: List Navigator -> Json -> Either String (List Json)
followPath (Key key isOptional : xs) v = 
  let err = if isOptional then Right Nil
                          else Left ("couldn't find (" <> key <> ") in " <> show v)
      result = preview (atKey key) v
   in (err `maybe` followPath xs) result
followPath (Traverse : xs) v = joinResults $ traverse (followPath xs) (v ^.. traverseArray)
followPath (Index n : xs) v = do
  case v ^? _Array <<< ix n of
       Nothing -> Left $ "index " <> show n <> " is out of bounds"
       Just v' -> followPath xs v'
followPath (Slice start end : xs) v =
  let elems = getSlice start end $ v ^.. _Array <<< traversed
   in followPath xs (fromArray <<< fromFoldable $ elems)
followPath Nil v = pure <<< pure $ v

getSlice :: forall a. Maybe Int -> Maybe Int -> List a -> List a
getSlice (Just start) (Just end) = take ((end - start) + 1) <<< drop start
getSlice Nothing (Just end) = take (end + 1)
getSlice (Just start) Nothing = drop start
getSlice Nothing Nothing = id

toArray :: List Json -> Json
toArray = fromArray <<< fromFoldable

atKey :: String -> Traversal' Json Json
atKey key = _Object <<< at key <<< _Just

traverseArray :: Traversal' Json Json
traverseArray = _Array <<< traversed

joinResults :: forall a f m. Functor f => Monad m => f (m (m a)) -> f (m a)
joinResults = map join

runBuilder :: Builder Path -> Json -> Either String (List Json)
runBuilder (BVal path) json = followPath path json
runBuilder (BList paths) json = map (pure <<< fromArray <<< fromFoldable) <<< joinResults $ traverse (\path -> runBuilder path json) paths
runBuilder (BObject obj) json = map fromObject <<< sequence <$>
                                traverse (\path -> runBuilder path json) obj
runBuilder (BPipes (expr : rest)) json = do
  results <- runBuilder expr json
  joinResults $ traverse (runBuilder (BPipes rest)) results
runBuilder (BPipes Nil) json = pure <<< pure $ json

crawl :: String -> Json -> Either String (List Json)
crawl path json = do
  p <- parseExpr path
  runBuilder p json
