{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE Haskell2010 #-}
module NativeDepComposeH2010 (checkH2010) where

import NativeDepComposeLib ((.), ($))
import Prelude (Bool, Int, (==), (+), (*), (&&))

checkH2010 :: Bool
checkH2010 =
     ((+ 1) . (* 2)) (5 :: Int) == 11
  && ((+ 3) $ (4 :: Int)) == 7
