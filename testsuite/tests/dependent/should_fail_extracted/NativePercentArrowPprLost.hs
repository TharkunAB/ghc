{-# LANGUAGE DataKinds, Modifiers, NoDependentTypes, PolyKinds, StandaloneKindSignatures, TypeFamilies, TypeOperators #-}
module NativePercentArrowPprLost where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)

type Recovered :: (a -> b) -> [a] -> [b]
type family Recovered f xs where
    Recovered f '[] = '[]
    Recovered f (x ': xs) = f x ': Recovered f xs

type Callback :: Type %U -> Type
type family Callback a where
    Callback Int = Char

lost :: Recovered Callback '[Int] :~: '[Char]
lost = Refl
