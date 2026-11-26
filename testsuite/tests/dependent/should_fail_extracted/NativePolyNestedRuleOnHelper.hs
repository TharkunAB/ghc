{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativePolyNestedRuleOnHelper (assocProdTo, badProof) where

import Data.Type.Equality ((:~:) (Refl))

assocPairR :: (a, b) -> c -> (a, (b, c))
assocPairR (x, y) z = (x, (y, z))
{-# NOINLINE assocPairR #-}
{-# RULES "assocPairR/alt" forall p z. assocPairR p z = (fst p, (snd p, z)) #-}

assocProdTo :: ((a, b), c) -> (a, (b, c))
assocProdTo (p, z) = assocPairR p z

badProof :: foreach (x :: a) -> foreach (y :: b) -> foreach (z :: c) ->
  assocProdTo '( '(x, y), z ) :~: '( x, '(y, z) )
badProof _ _ _ = Refl
