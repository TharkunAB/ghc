{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThCaptureNested where
import Data.Proxy (Proxy)
import Language.Haskell.TH

spelledCapture :: forall (b :: Bool). Proxy b ->
  $(pure (CaseT (VarT (mkName "b"))
    [ TypeCaseAlt (PromotedT 'True) (ConT ''Int)
    , TypeCaseAlt (PromotedT 'False) (ConT ''Char) ]))
spelledCapture = undefined

freshCapture :: forall (b :: Bool). Proxy b ->
  $(do b <- newName "b"
       pure (CaseT (VarT b) [TypeCaseAlt WildCardT (ConT ''Int)]))
freshCapture = undefined

rebindCapture :: forall (b :: Bool). Proxy b ->
  $(do Just b <- lookupTypeName "b"
       pure (CaseT (VarT b) [TypeCaseAlt (VarT b) (ConT ''Int)]))
rebindCapture = undefined
