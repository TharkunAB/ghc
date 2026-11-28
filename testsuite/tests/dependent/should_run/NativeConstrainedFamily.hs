{-# LANGUAGE FlexibleContexts, FlexibleInstances, GADTs, NoDependentTypes, PartialTypeSignatures, RequiredTypeArguments, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeConstrainedFamilyProvider (Allowed, Collects(..), Key, KeyElement, ListSource(..))
import qualified NativeConstrainedFamilyOrdProvider as OrdP
import qualified NativeConstrainedFamilyClient as CFC
import qualified NativePartialFamilyScope as PFS
import qualified NativeImprovedFamilyScope as IFS
import NativeFamilyEquationScope ()
import NativeDerivedFamilyScope (Wrapped(..), ViaList(..))

instance {-# OVERLAPPING #-} Allowed a => Collects (Key [a]) where
    type Elem (Key [a]) = Bool

aliasValue :: (KeyElement [Bool], KeyElement [Bool])
aliasValue = (True, True)

data Cell c where
    Cell :: Collects c => Proxy c -> Elem c -> Cell c

class Render a where
    render :: a -> String

instance (Collects c, Show (Elem c)) => Render (Cell c) where
    render (Cell _ x) = show x

class Collects c => Readable c
instance Readable [Bool]
newtype ReadableCell c = ReadableCell (Cell c)
instance (Readable c, Show (Elem c)) => Render (ReadableCell c) where
    render (ReadableCell cell) = render cell

newtype FixedCell a = FixedCell (Cell [a])
instance (a ~ Bool, Show (Elem [a])) => Render (FixedCell a) where
    render (FixedCell cell) = render cell

takeType :: forall a -> Proxy a
takeType (type _) = Proxy

partialChecks :: (Elem [Bool] :~: _, (), ())
partialChecks =
    ( Refl
    , Proxy @(Elem [Bool]) `seq` ()
    , takeType (type (Elem [Bool])) `seq` ()
    )

derivedProof :: SourceItem (ViaList Bool) :~: Bool
derivedProof = Refl

-- Ordinary provider control: unguarded family application succeeds when provider had NoDependentTypes.
ordinaryControl :: Proxy (OrdP.Elem [Int])
ordinaryControl = Proxy

main :: IO ()
main = do
    CFC.ground `seq` PFS.ascribed `seq` IFS.ground `seq`
        aliasValue `seq` partialChecks `seq` derivedProof `seq` ordinaryControl `seq` pure ()
    let cell = Cell (Proxy :: Proxy [Bool]) True
    print (render cell, render (ReadableCell cell), render (FixedCell cell))
    print (first (Wrapped [True]), first (ViaList [False]))
