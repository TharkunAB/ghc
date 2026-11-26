{-# LANGUAGE DependentTypes, ListTuplePuns, LambdaCase #-}
module NativeTypeCaseChangeHead where
import Prelude hiding (not)

data Field where
  Field :: (foreach (b :: Bool) -> (case not b of { True -> Int; False -> Char })) -> Field

value :: Field
value = Field (\case True -> 41; False -> 'f')

{-# NOINLINE not #-}
not :: Bool -> Bool
not True = True
not False = False
{-# RULES "change local head" forall x. not x = False #-}
