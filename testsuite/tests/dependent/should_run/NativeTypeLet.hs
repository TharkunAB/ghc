{-# LANGUAGE DataKinds, LinearTypes, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main where
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (Multiplicity(..))
import NativeTypeLetContexts ()
import NativeTypeLetOperator (Handler, Prefix, Result)
import NativeTypeLetOrdinary ()
import Data.Proxy (Proxy)
import PackedVecLibrary (Nat(..), Vec(..))
import TypeLet (Frame, frameRow, frameToList)

longer :: Proxy n -> Frame n 'True Char :~: Vec Char ('S n)
longer _ = Refl
shorter :: Proxy n -> Frame n 'False Char :~: Vec Char n
shorter _ = Refl
captured :: Result Either Char Bool 'True :~: Maybe (Either Char Bool)
captured = Refl
second :: Result (,) Char Bool 'False :~: [(Char, Bool)]
second = Refl
inferred :: Prefix Either Int Char 'True :~: Maybe (Either Int Char)
inferred = Refl
linear :: Handler 'One Char Bool 'True :~: Maybe (Char %1 -> Bool)
linear = Refl
many :: Handler 'Many Char Bool 'False :~: [Char -> Bool]
many = Refl

main :: IO ()
main = do
    let row = VCons 'x' (VCons 'y' VNil)
        render flag = frameToList @Char flag 2
            (frameRow flag 2 (if flag then ':' else error "unused header") row)
    print (render True, render False)
