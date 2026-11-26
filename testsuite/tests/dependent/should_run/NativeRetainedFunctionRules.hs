{-# LANGUAGE NoDependentTypes #-}
module Main where
import Data.IORef (newIORef, readIORef, writeIORef)
import NativeRetainedFunctionRulesCallback
import NativeRetainedFunctionRulesScopes ()
import NativeRetainedFunctionRulesUse (closed, open)
import RetainedRulesClient
import VisibleRuleClient

main :: IO ()
main = do
    cell <- newIORef True
    yes <- readIORef cell
    writeIORef cell False
    no <- readIORef cell
    print (fromInput yes, fromInput no, atFalse 11, ignoredIndex, boundPayload yes, boundPayload no)
    print obsBare
    print obsFamily
    print obsGiven

    m <- newIORef (Just True) >>= readIORef
    bottom <- newIORef (error "forced reading index" :: Maybe Bool) >>= readIORef
    other <- newIORef (error "forced Char reading index" :: Maybe Char) >>= readIORef
    ignored <- newIORef (error "forced first argument") >>= readIORef
    print (readAt m 37, readAt bottom 19, readAfter ignored m 43, readTagged ignored bottom 47,
           readRequired ignored bottom 53, readRequiredOther ignored other 59,
           readTwoRequired ignored bottom other 61, readTwoRequiredOther ignored other other 67)

    b <- newIORef True >>= readIORef
    print (closed 11, open b 17, open (error "unused index") 19)
