{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, ScopedTypeVariables, TypeOperators #-}
module Main where

import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeCaseOrderedScopeDomain (Pair(..))
import qualified NativeTypeCaseOrderedScopeConsume as OC
import qualified NativeTypeCaseOrderedScopeProvide as OP
import NativeTypeCaseScopedResultDomain (Choice(..))
import qualified NativeTypeCaseScopedResultConsume as C
import qualified NativeTypeCaseScopedResultProvide as P

observe :: Bool -> Bool -> Choice -> (String, String)
observe a b d = (C.sameField d (P.field d), C.sameCapture a b d (P.capture a b d))

atUnknown :: Maybe Bool -> String
atUnknown m = OC.render m (OP.pick m)

atJust :: Bool -> String
atJust b = OC.render (Just b) (OP.pick (Just b))

unforcedPairField :: forall (b :: Bool). Proxy b -> OC.Second ('Pair b 'False) :~: Int
unforcedPairField _ = Refl

unforcedJustField :: forall (b :: Bool). Proxy b -> OC.Lazy ('Just b) :~: Int
unforcedJustField _ = Refl

main :: IO ()
main = do
    cell <- newIORef Neither
    let check = observe (error "forced first capture") (error "forced second capture")
        nested a d = C.nested a d (P.nested a d)
    before <- check <$> readIORef cell
    writeIORef cell (Both (error "forced first field") (error "forced second field"))
    after <- check <$> readIORef cell
    nestedResult <- nested (error "forced nested capture") <$> readIORef cell
    print (before, after, nestedResult)

    m <- newIORef (Just (error "forced inner field")) >>= readIORef
    b <- newIORef (error "forced inner field") >>= readIORef
    print (atUnknown m, atJust b)
