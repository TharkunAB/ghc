{-# LANGUAGE DataKinds, Haskell2010, MultiParamTypeClasses, NoDependentTypes, TypeFamilies #-}
module Main (main) where

import Data.Proxy (Proxy(..))
import GHC.TypeLits (natVal)
import Numeric.Natural (Natural)
import qualified NativeScopedDictExportProvider as P
import NativeScopedDictImportCheck (runImportCheck)
import NativeScopedDictAmbigOk ()
import qualified ScopedFamilyMethodProvider as SF

data Normal = Normal Bool
data Invert = Invert Bool
data Ignore = Ignore
data Unobserved = Unobserved

instance P.Choose Normal where choose (Normal x) = x
instance P.Choose Invert where choose (Invert x) = not x
instance P.Choose Ignore where choose _ = True
instance P.Choose Unobserved where choose _ = error "the method was forced"

readChoice :: P.Choose a => a -> Bool
readChoice x = case P.chooseTagged x of
    P.Tagged b -> b

goodCombined :: Proxy (P.Element Bool)
goodCombined = Proxy

instance SF.Collects Int where
    type Element Int = Bool
    compareOrigin x = compare x 0

data AlwaysBelow = AlwaysBelow
instance SF.Collects AlwaysBelow where
    type Element AlwaysBelow = Char
    compareOrigin _ = LT

instance SF.Encode Char Bool where
    encode x = x == 'q'

readEntry :: SF.Collects c => c -> SF.Element c -> (Ordering, SF.Element c)
readEntry x payload = case SF.inspectEntry x payload of
    SF.TaggedEntry order item -> (order, item)

readEncoding :: SF.Collects c => c -> Char -> String
readEncoding x payload = case SF.encodeEntry x payload of
    SF.TaggedEntry _ text -> text

readPayload :: SF.Collects c => c -> a -> (Ordering, a)
readPayload x payload = case SF.inspectPayload x payload of
    SF.TaggedEntry order item -> (order, item)

measureFive :: SF.Collects c => c -> (Ordering, Natural)
measureFive x = case SF.measuredEntry x 5 of
    (SF.TaggedEntry order n, _) -> (order, n)

projectFive :: SF.Collects c => c -> Proxy 5
projectFive x = snd (id (SF.measuredEntry x 5))

checkFiveMatch :: SF.Collects c => c -> Proxy 5
checkFiveMatch x = case SF.measuredEntry x 5 of (_, p) -> p

main :: IO ()
main = do
    goodCombined `seq` checkFiveMatch True `seq` pure ()
    runImportCheck
    let input = True
    print (readChoice (Normal input), readChoice (Invert input))
    print (P.observe (Normal input))
    print (P.observeBoth (Normal input) (Invert input))
    print (P.observeGround input)
    print (P.observe (error "the ignored operand was forced" :: Ignore))
    print (P.ignorePayload Unobserved)
    print (readEntry (2 :: Int) True, readEntry (error "unforced" :: AlwaysBelow) 'x')
    print (readPayload (2 :: Int) 'q', fst (readPayload AlwaysBelow (error "unforced payload" :: Char)))
    print (readEncoding (2 :: Int) 'q', readEncoding AlwaysBelow 'x')
    let high = True
        low = False
        unused = error "forced unused local payload" :: Char
    print (case SF.inspectPayload high 'q' of SF.TaggedEntry order value -> (order, value))
    print (case SF.inspectPayload low unused of SF.TaggedEntry order _ -> order)
    print (measureFive True, measureFive False)
    print (natVal (projectFive True))
