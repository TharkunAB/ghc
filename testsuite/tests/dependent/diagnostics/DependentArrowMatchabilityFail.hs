{-# LANGUAGE DataKinds, GADTs, Modifiers, PolyKinds, ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
module DependentArrowMatchabilityFail where

import Data.Kind (Type)
import DependentArrowMatchability (AppliedDefault)
import qualified DependentArrowProvider as P
import GHC.Exts (FUN, M, U, Multiplicity(Many))

-- | Reject mismatched explicit matchability arguments.
type BadMaybeU = P.D (FUN @U 'Many Type Type) Maybe
type BadAbstractM = P.D (FUN @M 'Many Type Type) P.Abstract

-- | Reject an explicit annotation that conflicts with the subject.
type BadAnnotation = (P.Abstract :: Type %M -> Type)

-- | Reject unsaturated family application when DependentTypes is disabled.
type AppliedAbstract = AppliedDefault P.Abstract

-- | Reject conflicting matchability requirements on one binder.
type BadMixedArrows f = (f Bool, P.D (Type %M -> Type) f, P.D (Type %U -> Type) f)
