{-# LANGUAGE DependentTypes, GADTs, ListTuplePuns #-}
module ForeachConCallbackMatchability where

import Data.Kind (Type)
import Data.Void (Void, absurd)

type Not p = p -> Void

data Dec (p :: Type) where
  Yes :: p     -> Dec p
  No  :: Not p -> Dec p

eraseDec :: Dec p -> Bool
eraseDec (Yes _) = True
eraseDec (No  _) = False

tBool :: Bool -> Type
tBool True  = type ()
tBool False = type Void

toWitness :: foreach (d :: Dec p) -> tBool (eraseDec d) -> p
toWitness d = case d of
  Yes p -> \_ -> p
  No  _ -> \v -> absurd v

fromWitness :: foreach (d :: Dec p) -> p -> tBool (eraseDec d)
fromWitness d = case d of
  Yes _  -> \_ -> ()
  No  np -> \p -> np p

data Box p = MkBox (p -> Void)

isBox :: Box p -> Bool
isBox (MkBox _) = False

unBox :: foreach (b :: Box p) -> p -> tBool (isBox b)
unBox b = case b of
  MkBox np -> \p -> np p
