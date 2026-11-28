{-# LANGUAGE GHC2024 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes #-}
{-# LANGUAGE ListTuplePuns #-}
{-# LANGUAGE TypeFamilies #-}

module NativeDepComposeSymUnsatOk (useSymGiven) where

import Data.Type.Equality ((:~:) (Refl))
import NativeDepComposeListDefs (Nat (..), foldr, plus)
import Prelude ()

useSymGiven
  :: foreach (xs :: [Nat]) -> foreach (z :: Nat)
  -> plus (foldr plus Zero xs) z :~: foldr plus z xs
  -> foldr plus z xs :~: plus (foldr plus Zero xs) z
useSymGiven _ _ Refl = Refl
