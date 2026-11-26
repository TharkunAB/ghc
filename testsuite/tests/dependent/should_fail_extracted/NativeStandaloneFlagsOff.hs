{-# LANGUAGE DataKinds, NoDependentTypes, NoLiberalTypeSynonyms, UnsaturatedTypeFamilies #-}
module NativeStandaloneFlagsOff where

import qualified Prelude as P

both :: a -> (Bool, Char)
both x =
  let select y = const y x
  in (select True, select 'x')

retain :: foreach (b :: Bool) -> Bool
retain b = b

type Promoted = P.not 'P.False
