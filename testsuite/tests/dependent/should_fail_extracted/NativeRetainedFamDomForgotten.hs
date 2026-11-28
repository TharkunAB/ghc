{-# LANGUAGE DependentTypes, ListTuplePuns, RankNTypes, TypeFamilies #-}
module NativeRetainedFamDomForgotten where
import ConstrainedElements (Collects(..))
type Ignore a = forall b. Show b => b -> String
forgotten :: Ignore (Elem [Int])
forgotten x = show x
