{-# LANGUAGE GADTs, NoDependentTypes, NoLiberalTypeSynonyms, PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures, TemplateHaskell, TypeApplications, TypeOperators #-}
module NativeSynonymReifyClient (Copied, CopiedPlain, check, checkPlain) where

import Data.Type.Equality ((:~:)(Refl))
import GHC.Matchability (M)
import Language.Haskell.TH
import NativeSynonymReifyProvider (Apply)

$(do TyConI (TySynD _ binders rhs) <- reify ''Apply
     kind <- reifyType ''Apply
     let copied = mkName "Copied"
         plain = mkName "CopiedPlain"
         stripKind (KindedTV name visibility _) = PlainTV name visibility
         stripKind binder = binder
     pure [ KiSigD copied kind
          , TySynD copied (map stripKind binders) rhs
          , TySynD plain binders rhs
          ])

check :: Copied Maybe Bool :~: Maybe Bool
check = Refl

checkPlain :: CopiedPlain @M Maybe Bool :~: Maybe Bool
checkPlain = Refl
