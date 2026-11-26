{-# LANGUAGE DataKinds, ImpredicativeTypes, NoDependentTypes, TypeApplications #-}
module NativeSimplifierPackageCase (runPackageCase) where
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Data.Proxy (Proxy(..))
import qualified LexicalPackageRules as L
import WitnessSchema (Payload(..), RankedBuilder, configured)

ranked :: RankedBuilder
ranked = \b -> configured @True @Bool Proxy id b

fieldsBottom :: RankedBuilder
fieldsBottom = \b -> prepare b
  where
    prepare :: Bool -> Payload Int
    prepare b = Payload (error "callback value") (\value -> (value, b))

constructorBottom :: RankedBuilder
constructorBottom = \_ -> payload
  where
    payload :: Payload Int
    payload = error "callback constructor"

{-# NOINLINE runCase #-}
runCase :: RankedBuilder -> Bool -> (Int -> Int) -> Int
runCase builder b readInt = case builder b of
    Payload value call ->
        let (n, tag) = call value
        in L.readHidden (if tag then n else n + 1, readInt)

expectError :: String -> Int -> IO Bool
expectError message value = do
    result <- try @ErrorCall (evaluate value)
    pure $ case result of
        Left err -> takeWhile (/= '\n') (displayException err) == message
        Right _ -> False

runPackageCase :: IO ()
runPackageCase = do
    fieldForced <- expectError "callback value" (runCase fieldsBottom True id)
    constructorForced <- expectError "callback constructor" (runCase constructorBottom True (const 17))
    print (runCase ranked False (+ 1), runCase ranked True (+ 2),
           runCase fieldsBottom True (const 17), fieldForced, constructorForced)
