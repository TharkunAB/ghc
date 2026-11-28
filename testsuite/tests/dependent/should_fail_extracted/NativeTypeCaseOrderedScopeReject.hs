{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables, TypeOperators #-}
module NativeTypeCaseOrderedScopeReject where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeCaseOrderedScopeDomain (Pair(..))
import qualified NativeTypeCaseOrderedScopeConsume as C
import qualified NativeTypeCaseOrderedScopeProvide as P

badChanged :: Bool -> String
badChanged b = C.different (Just b) (P.pick (Just b))

badPair :: Pair -> String
badPair p = C.renderPair p (P.pickPair p)

badJust :: Bool -> String
badJust b = C.renderLazy (Just b) (P.pickEager (Just b))

badPartial :: Maybe Bool -> String
badPartial m = C.renderPartial m (P.pickPartial m)

badPairRigid :: forall (b :: Bool). Proxy b -> P.First ('Pair b 'False) :~: Int
badPairRigid _ = Refl

badJustRigid :: forall (b :: Bool). Proxy b -> P.Eager ('Just b) :~: Int
badJustRigid _ = Refl
