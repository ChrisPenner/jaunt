module Navigate where


import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List)
import Data.Maybe (Maybe)
import Data.StrMap (StrMap)
data Navigator = Key String Boolean | Traverse | Slice (Maybe Int) (Maybe Int) | Index Int

derive instance genericNavigator :: Generic Navigator _
instance eqNavigator :: Eq Navigator where
  eq = genericEq
instance showNavigator :: Show Navigator where
  show = genericShow

type Path = List Navigator

data Builder a = BPipes (List (Builder a)) | BList (List (Builder a)) | BObject (StrMap (Builder a)) | BVal a
derive instance genericBuilder :: Generic (Builder a) _

instance eqBuilder ::  Eq a => Eq (Builder a) where
    eq (BPipes p) (BPipes p') = eq p p'
    eq (BList l) (BList l')= eq l l'
    eq (BObject m) (BObject m') = eq m m'
    eq (BVal v) (BVal v')= eq v v'
    eq _ _ = false

instance showBuilder ::  Show a => Show (Builder a) where
    show (BPipes p) = "BPipes (" <> show p <> ")"
    show (BList l) = "BList (" <> show l <> ")"
    show (BObject m) = "BObject (" <> show m <> ")"
    show (BVal v) = "BVal (" <> show v <> ")"

derive instance functorBuilder :: Functor Builder
