{-# LANGUAGE FlexibleContexts, FlexibleInstances, NoDependentTypes, PartialTypeSignatures, ScopedTypeVariables, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module NativeConstrainedFamInstanceAndLocal where
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Collects(..))
newtype Ground = Ground Int
class Render a where render :: a -> String
instance Show (Elem [Int]) => Render Ground where
    render (Ground n) = show n
data Unqualified c = Unqualified (Proxy (Elem c))
missingGround :: Elem [Int] :~: Int
missingGround = Refl
partialGround :: Elem [Int] :~: _
partialGround = Refl
outer (_ :: Proxy c) =
    let local :: Proxy (Elem c)
        local = Proxy
    in local `seq` ()
