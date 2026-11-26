{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module NativeScopedDictImportCheck (runImportCheck) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.List (isInfixOf)
import NativeScopedDictExportProvider

data Inverted = Inverted Bool
instance Choose Inverted where choose (Inverted x) = not x

data Ignored = Ignored
instance Choose Ignored where choose _ = True

data Unobserved = Unobserved
instance Choose Unobserved where choose _ = error "the method was forced"

forward :: forall a. Choose a => foreach (x :: a) -> Tagged (choose x)
forward x = chooseTagged x

forwardBoth :: forall a b. (Choose a, Choose b)
            => foreach (x :: a) -> foreach (y :: b)
            -> (Tagged (choose x), Tagged (choose y))
forwardBoth x y = chooseTaggedBoth x y

readBoth :: (Choose a, Choose b) => a -> b -> (Bool, Bool)
readBoth x y = case forwardBoth x y of
    (Tagged lhs, Tagged rhs) -> (lhs, rhs)

readOne :: Choose a => a -> Bool
readOne x = case forward x of
    Tagged value -> value

discardPayload :: Choose a => a -> ()
discardPayload x = case forward x of
    Tagged _ -> ()

runCase :: Bool -> IO ()
runCase input = do
    print (readBoth input (Inverted input))
    print (readOne input)
    print (readOne (error "the ignored operand was forced" :: Ignored))
    print (discardPayload Unobserved)

runImportCheck :: IO ()
runImportCheck = do
    runCase True
    runCase False
    forced <- try (evaluate (readOne Unobserved)) :: IO (Either ErrorCall Bool)
    case forced of
        Left err | "the method was forced" `isInfixOf` show err -> pure ()
        other -> error ("Expected forced method error, got: " ++ show other)
