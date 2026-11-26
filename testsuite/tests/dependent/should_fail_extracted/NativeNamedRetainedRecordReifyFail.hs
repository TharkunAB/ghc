{-# LANGUAGE NoDependentTypes, TemplateHaskell #-}
module NativeNamedRetainedRecordReifyFail where

import qualified Language.Haskell.TH as TH
import NamedSiblingColumns (NamedColumns)

$(do TH.TyConI (TH.DataD cxt _ params kind [TH.RecC _ fields] derivs) <-
       TH.reify ''NamedColumns
     let record = TH.mkName "Recovered"
     pure [TH.DataD cxt record params kind [TH.RecC record fields] derivs])
