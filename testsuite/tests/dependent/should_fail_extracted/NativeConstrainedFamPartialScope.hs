{-# LANGUAGE AllowAmbiguousTypes, FlexibleContexts, GADTs, NamedWildCards, NoDependentTypes, PartialTypeSignatures, ScopedTypeVariables, StaticPointers, TemplateHaskell, TypeApplications, TypeFamilies #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
module NativeConstrainedFamPartialScope where
import Data.Proxy (Proxy(..))
import GHC.StaticPtr (StaticPtr)
import Language.Haskell.TH.Syntax (Code, Q)
import NativeConstrainedFamilyProvider (Collects(..))
data Witness c where
    Witness :: Collects c => Witness c
quoted :: forall t. Code Q ()
quoted = [|| () ||]
ctxWild :: forall c. _ => Proxy c -> _
ctxWild _ = Proxy @(Elem c) `seq` ()
staticEsc :: forall c. Collects c => Proxy c -> _
staticEsc _ = (static (Proxy @(Elem c) `seq` ()) :: StaticPtr ())
spliceEsc :: Proxy Int -> _
spliceEsc _ = ([|| \(w :: Witness Int) -> case w of Witness -> $$(quoted @(Elem Int)) ||] :: Code Q (Witness Int -> ()))
otherIdx :: Collects _index => Proxy _index -> Proxy other -> _
otherIdx _ (_ :: Proxy other) = Proxy @(Elem other) `seq` ()
inferredEsc :: forall c. Witness c -> _
inferredEsc Witness = Proxy @(Elem c)
