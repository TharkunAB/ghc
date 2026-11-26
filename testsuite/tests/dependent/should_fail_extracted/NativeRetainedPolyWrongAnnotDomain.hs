{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, TypeApplications, TypeOperators #-}
module NativeRetainedPolyWrongAnnotDomain where
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeRetainedPolyAndInlineGadtProvider as P
plugHoleId :: forall (t :: P.UTerm 'P.Zero).
              foreach (c :: P.UCtx 'P.Zero 'P.Zero) ->
              (c ~ 'P.CHole) =>
              P.plug c t :~: t
plugHoleId _ = Refl
bad = plugHoleId @(P.ULam (P.UVar P.FZ) :: P.UTerm 'P.Zero)
                 (P.CHole :: P.UCtx 'P.Zero ('P.Succ 'P.Zero))
