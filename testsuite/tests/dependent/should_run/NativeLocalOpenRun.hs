{-# LANGUAGE GADTs, NoDependentTypes #-}
module Main where
import GHC.Generics (from, to)
import IndependentFamily (Payload)
import qualified IndependentFamily
import LocalAliasField (AliasField(..), aliasField)
import qualified LocalAssociated
import qualified LocalDataFamily
import LocalInstance (passValue)
import NativeLocalOpenDerived (Later, Phase(..), Ticket(..), earlyLabel, label, lateLabel)

selectedAtBool :: Later Char Bool
selectedAtBool = to (from Dusk) > Noon

renderAlias :: Bool -> String
renderAlias b = case aliasField of
    AliasField choose -> case b of
        True -> show (choose b :: Int)
        False -> [choose b]

siblingPayload :: Payload Char
siblingPayload = EQ

main :: IO ()
main = do
    print (renderAlias False, renderAlias True)
    print (selectedAtBool, show (Ticket Dusk), label (Ticket Noon), earlyLabel (), lateLabel)
    print (passValue (7 :: Int), passValue True, LocalAssociated.result, LocalDataFamily.result)
    print (IndependentFamily.result, siblingPayload)
