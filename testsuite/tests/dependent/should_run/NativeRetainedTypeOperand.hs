{-# LANGUAGE DependentTypes, ListTuplePuns, NoGeneralizedNewtypeDeriving #-}
{-# LANGUAGE RequiredTypeArguments, Safe, ScopedTypeVariables, TypeApplications #-}
module NativeRetainedTypeOperand (runOperand) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import NativeRetainedTypeWitnessLib
import Type.Reflection (TypeRep, Typeable)

reflect :: forall a. Typeable a => a -> Maybe a
reflect = onlyBool (type a)

erased :: forall (a :: Type) -> ()
erased (type _) = ()

ignoreType :: forall (a :: Type). Proxy a -> ()
ignoreType _ = erased (type a)

runOperand :: IO ()
runOperand = do
  print (preserve (type Bool) True, reflect False)
  print (onlyBool (type Char) (error "unused payload"))
  print (ignoreType (Proxy @Char), (type Maybe) :: TypeRep Maybe)
  print (selected @{type Char} (error "unused payload"),
         preserveImplicit @{error "unused witness" :: TypeRep Bool} True)
