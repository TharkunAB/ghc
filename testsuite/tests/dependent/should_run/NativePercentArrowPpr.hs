{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativePercentArrowPpr (Map, Recovered, Callback, checked) where
import Data.Kind (Type)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (Matchability(Unmatchable), U)

type Map :: (a %Unmatchable -> b) -> [a] -> [b]
type family Map f xs where
    Map f '[] = '[]
    Map f (x ': xs) = f x ': Map f xs

type Recovered :: (a %Unmatchable -> b) -> [a] -> [b]
type family Recovered f xs where
    Recovered f '[] = '[]
    Recovered f (x ': xs) = f x ': Recovered f xs

type Callback :: Type %U -> Type
type family Callback a where
    Callback Int = Char

checked :: Recovered Callback '[Int] :~: '[Char]
checked = Refl
