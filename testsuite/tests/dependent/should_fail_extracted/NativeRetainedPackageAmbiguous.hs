{-# LANGUAGE NoDependentTypes, RankNTypes, TypeFamilies #-}
module NativeRetainedPackageAmbiguous where
import ConservedPartition (Branch, Split)
import Data.Functor.Const (Const(..))
import Data.Kind (Type)
import DependentPair (Consumer)

type family Hidden a :: Type
ambigConsumer :: Consumer (Hidden a) (Const Bool) Bool
ambigConsumer _ (Const b) = b

chooseAmbiguous :: Split a total -> Branch a r -> r
chooseAmbiguous = undefined
