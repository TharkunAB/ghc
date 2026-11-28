{-# LANGUAGE DataKinds, ExplicitNamespaces, GADTs, NoDependentTypes, PolyKinds, RankNTypes, RequiredTypeArguments, ScopedTypeVariables, TypeApplications, TypeOperators #-}
module NativeRetainedPackageClients
  ( FlagG(..), runGeneral, runRuntimeProjection, runQuantified, runLinked, runListVector, runDefaultO1
  , Holder(..), viaAlias, viaDirect, oneEq, zeroEq
  ) where

import ConservedPartition (Split, branchContents, partitionVec)
import Control.Exception (ErrorCall, displayException, evaluate, try)
import Control.Monad (unless)
import Data.Functor.Const (Const(..))
import Data.IORef (newIORef, readIORef)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (isJust, isNothing)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl), (:~~:)(HRefl))
import DependentPair (Consumer, DPair(..), first, withPair)
import qualified DependentProjection as DP
import qualified GHC.Sigma as S
import qualified GHC.Sigma.Projection as SP
import ListVectorClient
import qualified NativeDefaultO1InterfaceProducer as O1
import NativeRetainedPackageProviders
import PackedProjection (Length, One, payload)
import PackedVecLibrary (Add, CheckedSplit, Nat(..), PackedVec(..), Vec(..), appendNative, appendVec, dropVec, genericRefinedNested, natToInt, packList, replicateNative, splitAtVec, splitAtVecChecked, takeVec, toList)
import qualified QuantifiedGadtIndex as QG
import StoredVector (ListVector)

data FlagG b where
  YesG :: FlagG 'True
  NoG  :: FlagG 'False

packG :: a -> r -> DPair a (Const r)
packG x val = Pair x (Const val)

readIntG :: DPair Int (Const Int) -> Int
readIntG (Pair _ (Const val)) = val

readFlagG :: DPair Bool FlagG -> Bool
readFlagG q = case DP.second q of
  YesG -> True
  NoG  -> False

type AsWritten t = t

readTagG :: AsWritten (Consumer a (Const Char) Char)
readTagG _ c = getConst c

readActionG :: Consumer (IO Int) (Const Char) Char
readActionG action tag = readTagG action tag

valueG :: DPair a (Const r) -> r
valueG q = getConst (DP.second q)

runGeneral :: IO ()
runGeneral = print (readIntG (packG (error "unused integer witness") 7),
                    withPair (packG (error "unused IO" :: IO Int) 'i') readActionG,
                    valueG (packG (error "unused function" :: Int -> Int) 'f'),
                    first (packG (12 :: Int) (error "unused payload" :: Char)),
                    readFlagG (Pair True YesG), readFlagG (Pair False NoG))

data Holder = Holder !(DPair Bool (Const Int))

readFlagS :: Flag b -> Int
readFlagS (Yes n) = n
readFlagS No = 0

viaAlias :: Aliased -> Int
viaAlias box = let pair = getAlias box in readFlagS (SP.second pair)

viaDirect :: Direct -> Int
viaDirect box = let pair = getDirect box in readFlagS (SP.second pair)

oneEq :: Length One :~: 'S 'Z
oneEq = Refl
zeroEq :: Length ('PackVec 'Z ('VNil :: Vec Bool 'Z)) :~: 'Z
zeroEq = Refl

runRuntimeProjection :: IO ()
runRuntimeProjection = do
    let consume p = toList (payload p)
        fields n = PackVec n (error "payload") :: PackedVec Int
    case consume (packList [7 :: Int]) of
      [7] -> pure ()
      _ -> fail "wrong vector"
    outcome <- try (evaluate (consume (fields (error "witness"))))
    case outcome of
      Left e | "payload" `isPrefixOf` displayException (e :: ErrorCall) ->
        putStrLn "the projected payload leaves the witness lazy"
      _ -> fail "wrong field evaluated"

clientTransport :: forall k (f :: forall (a :: k). Proxy a -> *).
  QG.Pick @k f -> QG.Carrier @k f -> QG.Carrier @Bool QG.Slot
clientTransport QG.PickBool x = x

runQuantified :: IO ()
runQuantified = do
    let obs (QG.Carrier n) = n
    unless ((obs (QG.transport QG.PickBool (QG.Carrier 41)),
             obs (clientTransport QG.PickBool (QG.Carrier 43))) == (41, 43)) $
        fail "quantified index mismatch"

type AtZero = Carrier ('VNil :: Vec Bool 'Z)

unpackLinked :: Linked n p q r -> (AtZero, AtZero)
unpackLinked (Linked (S.Pair _xs (S.Pair _ys (S.Pair _zs (S.Pair _a (S.Pair _b
    (p1, p2, p3, branch, leaf, back, prefix, pay))))))) =
    case p1 of
        HRefl -> case p2 of
            Refl -> case p3 of
                HRefl -> case branch of
                    HRefl -> case leaf of
                        HRefl -> case back of
                            HRefl -> (prefix, pay)

runLinked :: IO ()
runLinked = do
    val <- newIORef (41 :: Int) >>= readIORef
    case unpackLinked (mkLinked val) of
        (Carrier prefix, Carrier result) | (prefix, result) == (41, 43) -> pure ()
        _ -> fail "linked index mismatch"

onlyEmpty :: forall n. Vec Bool n -> Maybe (Vec Bool 'Z)
onlyEmpty xs = case matchZero @n xs of
    Just HRefl -> Just xs
    Nothing -> Nothing

vectorProofs :: Vec Int n -> (Bool, Bool)
vectorProofs xs =
    (case vectorRefl xs of Refl -> True, case forwardVector xs of Refl -> True)

rejoin :: Vec a cut -> Vec a suffix -> (Add cut suffix :~: total) -> Vec a total
rejoin left right Refl = appendVec left right

deferProof :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Maybe [Bool]
deferProof (S.Pair Z ~(_, _, proof)) later = case proof of
    Refl -> case later of
        (next :: Vec Bool ('S 'Z)) -> Just (toList next)
deferProof _ _ = Nothing

ignoreTuple :: CheckedSplit Bool ('S 'Z) total -> Vec Bool total -> Bool
ignoreTuple (S.Pair _ ~(_, _, _)) _ = True

runListVector :: IO ()
runListVector = do
    let observe :: ListVector Char -> (Int, String)
        observe = contents
        empty :: Split Int 'Z
        empty = partitionVec (error "unused empty predicate") (VNil :: Vec Int 'Z)
        onlyKept, onlyDropped :: Split Int ('S 'Z)
        onlyKept = S.Pair (S Z) (S.Pair Z (VCons 7 VNil, error "unused dropped partition", error "unused sum proof"))
        onlyDropped = S.Pair Z (S.Pair (S Z) (error "unused kept partition", VCons 9 VNil, Refl))
    print (observe (fromList ""), observe (fromList "ab"),
           take 1 (snd (observe (fromList ('x' : error "unused tail")))),
           fst (observe (fromList [error "unused element" :: Char])),
           fromList (error "unused list" :: String) `seq` True)
    let mixed = regroupObservation (> (2 :: Int)) [3, 1, 4, 2]
        lazy = length (snd (regroupObservation (const True) [error "unused partition element" :: Int]))
        selected :: [(Int, [Int])]
        selected =
            [ choosePartition (type 'Z) True empty branchContents
            , choosePartition (type 'Z) False empty branchContents
            , choosePartition (type ('S 'Z)) True onlyKept branchContents
            , choosePartition (type ('S 'Z)) False onlyDropped branchContents
            ]
        split = case splitAtVec (S Z) (VCons (7 :: Int) (VCons 9 VNil)) of
            (left, right) -> (toList left, toList right)
        zeroVector = splitAtVec Z (error "unused vector" :: Vec Int ('S 'Z))
        zeroElement = splitAtVec Z (VCons (error "unused split element" :: Int) VNil)
        one = splitAtVec (S Z) (VCons (error "unused prefix element" :: Int)
                                     (error "unused suffix" :: Vec Int ('S 'Z)))
        demand = (length (toList (fst zeroVector)), length (toList (snd zeroElement)),
                  length (toList (fst one)))
        proofs = (vectorProofs (VCons 7 VNil),
                  vectorProofs (error "unused indexed vector" :: Vec Int 'Z))
        zero = VNil :: Vec Bool 'Z
        nonzero = VCons (error "unused head") (error "unused tail" :: Vec Bool 'Z)
        knownZero = case zeroRefl zero of Refl -> True
        openZero = case matchZero zero of
            Just proof -> case reverseZero zero Refl proof of HRefl -> True
            Nothing -> False
        lazyCons = case matchZero nonzero of
            Nothing -> True
            Just _ -> False
        bottom = error "zero case demands the scrutinee" :: Vec Bool 'Z
    forced <- try (evaluate (case zeroRefl bottom of Refl -> True)) :: IO (Either ErrorCall Bool)
    let emptyResults = isJust (onlyEmpty zero) && isNothing (onlyEmpty nonzero)
        zeroChecks = knownZero && openZero && lazyCons && emptyResults
                  && either (const True) (const False) forced
    case (mixed, lazy, selected, split, demand, proofs, zeroChecks) of
        ((4, [3, 4, 1, 2]), 1, [(0, []), (0, []), (1, [7]), (1, [9])],
         ([7], [9]), (0, 1, 1), ((True, True), (True, True)), True) -> pure ()
        other -> fail (show other)
    cut <- newIORef (S (S Z)) >>= readIORef
    let joined = appendNative @Int cut (S Z) (replicateNative cut 7) (VCons 9 VNil)
        prefix = takeVec @('S 'Z) cut joined
        suffix = dropVec @('S 'Z) cut joined
        takeZero = takeVec Z (error "unused vector at zero cut" :: Vec Int ('S 'Z))
        dropZero = dropVec Z (VCons (9 :: Int) VNil)
        firstCell = take 1 (toList (takeVec @'Z (S (S Z))
                          (VCons 7 (error "unused rest of the prefix"))))
        onlyPrefix = takeVec (S Z) (VCons (7 :: Int) (error "unused suffix" :: Vec Int ('S 'Z)))
        onlySuffix = dropVec (S Z) (VCons (error "unused dropped element") (VCons (9 :: Int) VNil))
    case (toList prefix, toList suffix, toList takeZero, toList dropZero,
          firstCell, toList onlyPrefix, toList onlySuffix) of
        ([7, 7], [9], [], [9], [7], [7], [9]) -> pure ()
        other -> fail (show other)
    cuts <- newIORef [Z, S Z, S (S Z), S (S (S Z))] >>= readIORef
    input <- newIORef [7 :: Int, 9] >>= readIORef
    case (checkedSplitObservation Z ([] :: [Int]), map (`checkedSplitObservation` input) cuts) of
        (Just (0, [], []), [Just (2, [], [7, 9]), Just (1, [7], [9]), Just (0, [7, 9], []), Nothing]) -> pure ()
        other -> fail (show other)
    case fromList input of
        S.Pair total cells -> case splitAtVecChecked (S Z) total cells of
            Just (S.Pair _ (left, right, proof))
                | toList (rejoin left right proof) == input -> pure ()
            _ -> fail "checked split did not rejoin at the input length"
    bottomTotal <- newIORef (error "unused total for zero cut" :: Nat) >>= readIORef
    partialTotal <- newIORef (S (error "unused remainder length")) >>= readIORef
    excess <- newIORef (S (error "unused excessive cut tail")) >>= readIORef
    let zeroC = isJust (splitAtVecChecked Z bottomTotal (replicateNative bottomTotal (7 :: Int)))
        successC = isJust (splitAtVecChecked (S Z) partialTotal (replicateNative partialTotal (7 :: Int)))
        unusedVector = isJust (splitAtVecChecked (S Z) (S Z) (error "unused accepted vector" :: Vec Int ('S 'Z)))
        failureC = isNothing (splitAtVecChecked excess Z (error "unused rejected vector" :: Vec Int 'Z))
        leftOnly = case splitAtVecChecked (S Z) (S (S Z)) (VCons (7 :: Int) (error "unused suffix")) of
            Just (S.Pair _ (left, _, _)) -> toList left
            Nothing -> []
        rightOnly = case splitAtVecChecked (S Z) (S (S Z)) (VCons (error "unused prefix") (VCons (9 :: Int) VNil)) of
            Just (S.Pair _ (_, right, _)) -> toList right
            Nothing -> []
    case (zeroC, successC, unusedVector, failureC, leftOnly, rightOnly) of
        (True, True, True, True, [7], [9]) -> pure ()
        other -> fail (show other)
    cell <- newIORef (VCons True VNil)
    let obtainSplit = splitAtVecChecked (S Z) (S Z) <$> readIORef cell
        later = VCons False VNil
    actual <- checkedSplitIO obtainSplit later
    delayed <- fmap (>>= \pair -> deferProof pair later) obtainSplit
    case (actual, delayed) of
        (Just ([True], [False]), Just [False]) -> pure ()
        other -> fail (show other)
    let badTuple = S.Pair Z (error "split tuple forced") :: CheckedSplit Bool ('S 'Z) ('S 'Z)
        badProof = S.Pair Z (VCons True VNil, VNil, error "split proof forced")
                   :: CheckedSplit Bool ('S 'Z) ('S 'Z)
        checkBottom message action = do
            result <- try @ErrorCall action
            case result of
                Left err | takeWhile (/= '\n') (displayException err) == message -> pure ()
                _ -> fail ("expected " ++ message)
    if ignoreTuple badTuple later then pure () else fail "lazy tuple did not match"
    checkBottom "split tuple forced" (checkedSplitIO (pure (Just badTuple)) later >>= evaluate)
    checkBottom "split proof forced" (evaluate (deferProof badProof later))

observeFlagO1 :: (Bool -> Bool) -> Bool -> Char
observeFlagO1 f b = case O1.classify f b of
    O1.Off -> 'f'
    O1.On  -> 't'

runDefaultO1 :: IO ()
runDefaultO1 = do
    n <- newIORef (5 :: Int) >>= readIORef
    unless ((O1.exposure (error "type-only index") (error "unused retained argument") n,
             O1.ordinaryWorker n (error "unused ordinary argument")) == (9, 11)) $
        fail "O1 worker mismatch"
    bad <- try (evaluate (O1.ordinaryWorker (error "strict ordinary argument") False))
        :: IO (Either ErrorCall Int)
    case bad of
        Left err | "strict ordinary argument" `isInfixOf` show err -> pure ()
        _ -> fail "strict argument should be forced"
    ref <- newIORef (error "unused IO witness" :: IO Int)
    w <- readIORef ref
    let pair = Pair w (Const 'i') :: DPair (IO Int) (Const Char)
    unless (getConst (DP.second pair) == 'i') (fail "second pair mismatch")
    let values :: [Vec Int ('S 'Z)]
        values = [VCons 19 VNil]
    unless (map toList (toList (genericRefinedNested (S Z) values)) == [[19]]) $
        fail "genericRefinedNested mismatch"
    let chars = Pair (S Z) (VCons 'x' VNil) :: DPair Nat (Vec Char)
    firstPair <- readIORef =<< newIORef chars
    secondPair <- readIORef =<< newIORef pair
    unless ((withPair firstPair (\x pay -> (natToInt x, toList pay)),
             withPair secondPair (\_ pay -> getConst pay)) == ((1, "x"), 'i')) $
        fail "withPair mismatch"
    strict <- try (evaluate (observeFlagO1 id (error "forced flag"))) :: IO (Either ErrorCall Char)
    unless ((observeFlagO1 not True, observeFlagO1 (const True) (error "unused input"),
             either (isInfixOf "forced flag" . show) (const False) strict) == ('f', 't', True)) $
        fail "observeFlag mismatch"
