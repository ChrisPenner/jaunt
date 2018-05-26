module Traverse where

import Prelude

import Data.Argonaut (Json, _Array, _Object, fromArray)
import Data.Array (fromFoldable)
import Data.Either (Either(..))
import Data.Foldable (class Foldable)
import Data.Lens (Traversal', _Just, preview, to, traversed, (^..), (^?))
import Data.Lens.At (at)
import Data.Lens.Index (ix)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..), maybe)
import Navigate (Navigator(..))
import Parse (parsePath)

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

