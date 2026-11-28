{-# LANGUAGE DataKinds, NoDependentTypes, TypeFamilies, TypeOperators #-}
module WrongTitle where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:) (Refl))
import UnicodeExport (Title)

type family Classify (c :: Char) :: Bool where
    Classify '\x1c5' = 'True
    Classify _ = 'False

wrong :: Title '\x1c6' :~: '\x1c4'
wrong = Refl

unknown :: Proxy c -> Classify (Title c) :~: 'False
unknown _ = Refl
