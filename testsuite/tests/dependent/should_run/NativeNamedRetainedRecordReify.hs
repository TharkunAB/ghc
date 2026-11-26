{-# LANGUAGE NoDependentTypes, TemplateHaskell #-}
module NativeNamedRetainedRecordReify (observe) where

import qualified Language.Haskell.TH as TH

data Ordinary = Ordinary { ordinaryField :: Bool }

$(do TH.TyConI (TH.DataD cxt _ params kind [TH.RecC _ [(_, bang, ty)]] derivs) <-
       TH.reify ''Ordinary
     let record = TH.mkName "Recovered"
         field = TH.mkName "recoveredField"
     pure [TH.DataD cxt record params kind [TH.RecC record [(field, bang, ty)]] derivs])

observe :: Bool
observe = recoveredField (Recovered { recoveredField = True })
