module Navigate where


import Data.List (List)
import Data.StrMap (StrMap)
import Prelude (class Functor, class Show)


data Navigator = Key String | Traverse | Slice Int Int | Index Int
derive instance showNavigator :: Show Navigator

type Path = List Navigator
derive instance showPath :: Show Path

data Builder a = BPipes (List (Builder a)) | BList (List (Builder a)) | BObject (StrMap (Builder a)) | BVal a
derive instance showBuilder ::  Show a => Show (Builder a)
derive instance functorBuilder :: Functor Builder
