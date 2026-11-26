{-# LANGUAGE NoDependentTypes, TemplateHaskell #-}
module NativeTypeLetQuoteDisabled where
import Language.Haskell.TH
type Bad = $(do b <- newName "b"; letT b (conT ''Int) (varT b))
