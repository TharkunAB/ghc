{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenPreludeZipWithFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenZipDemandControls (twiceMinimum)
import PreludeMapLengthProof (Count)
import PreludeMapLengthSize (Nat(..), size)
import PreludeZipSource (ZipPairs)
import PreludeZipWithSource (Zipped)

wrongMin :: forall a b. foreach (xs :: [a]) -> foreach (ys :: [b])
         -> size (zip xs ys) :~: twiceMinimum (size xs) (size ys)
wrongMin [] _ = Refl
wrongMin (_ : _) [] = Refl
wrongMin (_ : xs) (_ : ys) = case wrongMin xs ys of
  Refl -> Refl

unknownLeft :: Proxy f -> Proxy xs -> Zipped f xs '[] :~: '[]
unknownLeft _ _ = Refl

dropCons :: Proxy f -> Proxy x -> Proxy y -> Proxy xs -> Proxy ys
         -> Zipped f (x ': xs) (y ': ys) :~: Zipped f xs ys
dropCons _ _ _ _ _ = Refl

unknownLeftLen :: Proxy f -> Proxy xs -> Count (Zipped f xs '[]) :~: 'Zero
unknownLeftLen _ _ = Refl

dropSucc :: Proxy f -> Proxy x -> Proxy y -> Proxy xs -> Proxy ys
         -> Count (Zipped f (x ': xs) (y ': ys)) :~: Count (Zipped f xs ys)
dropSucc _ _ _ _ _ = Refl

unknownZipLeft :: Proxy xs -> ZipPairs xs '[] :~: '[]
unknownZipLeft _ = Refl

dropZipCons :: Proxy x -> Proxy xs -> Proxy y -> Proxy ys
            -> ZipPairs (x ': xs) (y ': ys) :~: ZipPairs xs ys
dropZipCons _ _ _ _ = Refl
