{-# LANGUAGE GHC2024, NoDependentTypes #-}
module NativeRetainedConstructorContextPlain (runKnown, runPlain) where

import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import Provider (Nat(..), Stored(..), T(..))

proof :: T a -> a :~: Int
proof (Mk _ _) = Refl

answer :: Int
answer = case proof (Mk (S Z) Proxy :: T Int) of Refl -> 1

runKnown :: IO ()
runKnown = do
    print answer
    let n = error "forced retained index" :: Nat
    case proof (Mk n (error "forced dependent payload") :: T Int) of
        Refl -> print (2 :: Int)

readBack :: Stored a b -> (a :~: Bool, b :~: Char, String)
readBack (Stored True n) = (Refl, Refl, show n)
readBack (Stored False c) = (Refl, Refl, [c])

ignorePayload :: Stored a b -> (a :~: Bool, b :~: Char)
ignorePayload (Stored _ _) = (Refl, Refl)

lazyStored :: Bool -> Stored Bool Char
lazyStored flag = Stored flag undefined

runPlain :: IO ()
runPlain = case (readBack (Stored True 71 :: Stored Bool Char),
                 readBack (Stored False 'r' :: Stored Bool Char),
                 ignorePayload (lazyStored undefined)) of
  ((Refl, Refl, a), (Refl, Refl, b), (Refl, Refl)) -> print (a, b)
