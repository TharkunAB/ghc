{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns #-}
module NativeRetainedPolyWrongAnnotCtor where
import qualified NativeRetainedPolyAndInlineGadtProvider as P
acceptNonDiag :: foreach (c :: P.UCtx 'P.Zero ('P.Succ 'P.Zero)) -> ()
acceptNonDiag _ = ()
bad :: ()
bad = acceptNonDiag (P.CHole :: P.UCtx 'P.Zero ('P.Succ 'P.Zero))
