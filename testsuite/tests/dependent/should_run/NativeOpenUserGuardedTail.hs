{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenUserGuardedTailAlias (Negated, Remainder)
import NativeOpenUserGuardedTailLib (dropOneIf)

removed :: Proxy xs -> Remainder Negated ('False ': xs) :~: xs
removed _ = Refl

preserved :: Proxy xs -> Remainder Negated ('True ': xs) :~: ('True ': xs)
preserved _ = Refl

main :: IO ()
main = do
  demand <- try (evaluate (dropOneIf (const True) (undefined : error "selected tail" :: [Bool])))
  print ( removed (Proxy @'[]), preserved (Proxy @'[])
        , take 1 (dropOneIf (const True) (undefined : True : error "forced later tail"))
        , hasCons (dropOneIf (const False) (undefined : undefined :: [Bool]))
        , case demand of Left (ErrorCall "selected tail") -> True; _ -> False )
  where
    hasCons (_ : _) = True
    hasCons [] = False
