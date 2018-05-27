module Navigate where


import Data.List (List)
import Data.StrMap (StrMap)
import Prelude (class Functor)


data Navigator = Key String | Traverse | Index Int

type Path = List Navigator

data Builder a = BList (List (Builder a)) | BObject (StrMap (Builder a)) | BNode a

derive instance functorBuilder :: Functor Builder
