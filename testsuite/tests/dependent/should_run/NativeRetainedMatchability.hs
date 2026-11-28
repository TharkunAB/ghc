{-# LANGUAGE ExplicitNamespaces, NoDependentTypes, RequiredTypeArguments, TypeApplications #-}
module Main where
import Control.Exception (SomeException, displayException, evaluate, try)
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf)
import GHC.Exts (U)
import qualified NativeRetainedMatchabilityDefer as Defer
import qualified NativeRetainedMatchabilityOrdinary as Ordinary
import qualified NativeRetainedMatchabilityRetained as Retained

threw :: IO a -> IO Bool
threw action = either (const True) (const False) <$> try @SomeException action

deferred :: IO a -> IO Bool
deferred action = either (isInfixOf "deferred type error" . displayException) (const False) <$> try @SomeException action

main :: IO ()
main = do
    tag <- newIORef (error "tag") >>= readIORef
    index <- newIORef (error "index" :: Maybe Bool) >>= readIORef
    prefix <- newIORef (error "strict prefix") >>= readIORef
    let result = case Retained.forward @U tag (type Bool) index (Retained.Reading 41) of Retained.Reading n -> n
        partial = Retained.strictPrefix @U tag (type Bool) index (Retained.Reading 0) prefix
        constructorPartial = Retained.constructor @U tag index (Retained.Reading 0)
        bad = case Defer.badRetained tag (type Bool) index (Retained.Reading 41) of Retained.Reading n -> n
    partialForced <- threw (evaluate (partial `seq` ()))
    fullForced <- threw (evaluate (partial 61))
    constructorPartialForced <- threw (evaluate (constructorPartial `seq` ()))
    constructorFullForced <- threw (evaluate (constructorPartial 61))
    ordinaryDeferred <- deferred (evaluate (Defer.badOrdinary 41))
    retainedDeferred <- deferred (evaluate bad)
    constructorDeferred <- deferred (evaluate (Defer.badConstructor True (Retained.Reading 0) 61))
    print (Ordinary.forward @U 41, result, Retained.constructor @U True index (Retained.Reading 0) 61)
    print (partialForced, fullForced, constructorPartialForced, constructorFullForced)
    print (ordinaryDeferred, retainedDeferred, constructorDeferred)
