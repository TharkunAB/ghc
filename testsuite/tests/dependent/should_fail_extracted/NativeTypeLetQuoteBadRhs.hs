{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeLetQuoteBadRhs where
import Language.Haskell.TH
type BadRhs = $(do b <- newName "b"; letT b (appT (conT ''Int) (conT ''Bool)) (conT ''Int))
type NonPair = $([t| let (_, _) = Bool in Int |])
type NonNestedPair = $([t| let ((_, _), _) = '(Bool, Char) in Int |])
