{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Main where
import Data.Kind (Type)
import GHC.Exists
import NativeSimplifierClients (runSimplifierClient, runLocalRule, runPackageRules)
import NativeSimplifierPackageCase (runPackageCase)

data SpecPayload a = SpecPayload a (a -> Int)
type Hidden = exists (a :: Type) | SpecPayload a

hideSpec :: Int -> Hidden
hideSpec x = Pack (type Int) (SpecPayload x id)

loop :: Int -> Maybe a -> (a -> Int) -> Int
loop 0 Nothing _ = 0
loop 0 (Just a) k = k a
loop n Nothing k = loop (n - 1) Nothing k
loop n (Just a) k = loop (n - 1) (Just a) k

readN :: Hidden -> Int -> Int
readN p n = case p of
  Pack (type t) (SpecPayload x k) -> loop @t n (Just x) k

main :: IO ()
main = do
    runSimplifierClient
    runLocalRule
    runPackageRules
    runPackageCase
    print (readN (hideSpec 11) 8)
