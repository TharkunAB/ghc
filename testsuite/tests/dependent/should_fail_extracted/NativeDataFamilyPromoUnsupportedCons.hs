{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, MagicHash, TypeFamilies #-}
module NativeDataFamilyPromoUnsupportedCons where
import TupleProvider
context :: At ('MkC 'True)
context = At
newtypeCon :: At ('MkN 3#)
newtypeCon = At
