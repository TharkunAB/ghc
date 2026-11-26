{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications #-}
module Main where
import AlignedList (zipWithExact)
import Control.Exception (ErrorCall, SomeException, displayException, evaluate, try)
import Control.Monad (unless)
import CursorClient (observeBoth, observeZip, observeAligned, observeCursors, observeIndexedCallback, zipTwoPairs, zipInt8Literals)
import Data.IORef (newIORef, readIORef)
import Data.Maybe (isJust)
import Data.Proxy (Proxy(..))
import FinitePositionClient (lookupVecViaView, observeConstructedViews, observeEmptyIndex, observeFinitePosition, observeListIndices)
import NativeGeneralGadtOperandProvider (Both(..), make)
import NativeIndexedConsProof (Seen(..), see, inlineCons, inlineNestedCons, inlineNil, writtenCons, writtenNil)
import NativeIndexedFinExtra (CheckedList(..), Tagged(..), Cell(..), sameScoped)
import NativeIndexedFinGate (Gate(..), Ticket(..), grant)
import NativeIndexedOperandWire (tagCons)
import NativeLinearIndexColumns (choose)
import NativeRetainedAsPattern (lookupBoth)
import PackedVecLibrary (Nat(..), Vec(..), appendNative, replicateNative, takeVec, toList)
import ParameterizedIndexLibrary (indexVec, lookupVecChecked, mkFinGeneric, mkFinVec)

written :: [Int]
written = toList (zipWithExact [1,2] [10,20] (+))

scopedVector :: Vec Int ('S ('S 'Z))
scopedVector =
    let a = 1 :: Int; b = 2 :: Int; c = 10 :: Int; d = 20 :: Int
    in zipWithExact [a,b] [c,d] (+)

scopedLambda :: [Int]
scopedLambda =
    toList ((\ ~a ~b ~c ~d -> zipWithExact [a,b] [c,d] (+)) 1 2 10 20)

roundList :: CheckedList xs -> CheckedList xs
roundList (CheckedList stored) = CheckedList stored

roundTag :: Tagged tag -> Tagged tag
roundTag (Dynamic stored) = Dynamic stored
roundTag Fixed = Fixed

roundCell :: Cell layout -> Cell layout
roundCell (Cell stored payload) = Cell stored payload

readLayout :: Cell layout -> [Bool]
readLayout (Cell stored _) = stored

hasPayload :: Cell layout -> Bool
hasPayload (Cell _ 37) = True
hasPayload (Cell _ _) = False

observedCell :: [Bool] -> Int -> [Bool]
observedCell layout payload = readLayout (roundCell (Cell layout payload))

dynamic :: Tagged tag -> Bool
dynamic (Dynamic _) = True
dynamic Fixed = False

runResultClient :: IO ()
runResultClient = do
    xs <- newIORef (error "forced retained list" :: [Bool]) >>= readIORef
    tag <- newIORef (error "forced retained ordering" :: Ordering) >>= readIORef
    layout <- newIORef [True] >>= readIORef
    payload <- newIORef (37 :: Int) >>= readIORef
    unusedPayload <- newIORef (error "forced ordinary payload" :: Int) >>= readIORef
    let listLazy = case roundList (CheckedList xs) of CheckedList _ -> True
        tagLazy = case roundTag (Dynamic tag) of
            Dynamic _ -> True
            Fixed -> False
    unless (listLazy && tagLazy && not (dynamic (roundTag Fixed))
            && observedCell layout unusedPayload == [True]
            && hasPayload (roundCell (Cell xs payload))
            && sameScoped) $
        fail "retained constructor result changed its runtime witness"

runMultiIndex :: IO ()
runMultiIndex = do
    let fixed :: (Both (Maybe Bool) [Ordering], Proxy ('Mk2 'LT 'False))
        fixed = make False LT
        lazy :: Bool -> Char -> Bool
        lazy x y = case fst (make x y) of Mk2 _ _ -> True
    case (fixed, lazy undefined undefined) of
        ((Mk2 LT False, Proxy), True) -> pure ()
        _ -> fail "wrong indexed constructor fields"

observesCons :: Vec Bool ('S 'Z) -> Bool
observesCons input = case see input of SeenCons -> True

runConsProof :: IO ()
runConsProof = do
    let headField = error "forced head" :: Bool
        secondHead = error "forced second head" :: Bool
        tailField = error "forced tail" :: Vec Bool 'Z
        lazyFields = VCons headField tailField
    unless (observesCons lazyFields) (fail "cons proof changed field demand")
    case inlineNil of SeenNil -> pure ()
    case inlineCons headField tailField of SeenCons -> pure ()
    case inlineNestedCons headField secondHead tailField of SeenCons -> pure ()
    case writtenNil of SeenNil -> pure ()
    case writtenCons headField tailField of SeenCons -> pure ()
    case fst (tagCons headField tailField) of VCons _ _ -> pure ()
    result <- try (evaluate (observesCons (error "forced scrutinee")))
              :: IO (Either SomeException Bool)
    case result of
      Left _ -> putStrLn "retained VCons preserves demand"
      Right _ -> fail "case stopped inspecting its scrutinee"

main :: IO ()
main = do
    observeFinitePosition >>= print
    observeListIndices >>= print
    let lazyOperands =
            take 1 (toList ((zipWithExact @Int @Int @Int [5 + 1, error "forced later left field"])
                [10, error "forced later right field"] (+))) :: [Int]
    unless ((written, toList scopedVector, scopedLambda, lazyOperands, zipInt8Literals) ==
            ([11,22], [11,22], [11,22], [16], [137,-108])) $
        fail "the retained list spine changed a field or closed result"
    both <- observeBoth
    unless (both == [Just ('b', 10), Just ('b', 20), Nothing]) (fail (show both))
    zipped <- observeZip
    unless (zipped == (Just [('a', 10), ('b', 20)], Nothing, Just 30, [('x', 'x')])) (fail (show zipped))
    case zipTwoPairs 'a' (error "forced second left field") True (error "forced second right field") of
        ('a', True) : _ -> pure ()
        _ -> fail "wrong first pair"
    aligned <- observeAligned
    unless (aligned == [Just ('b', 20), Nothing, Nothing]) (fail (show aligned))
    callbacks <- observeIndexedCallback
    unless (callbacks == (Just "cc", Nothing, True)) (fail (show callbacks))
    word <- newIORef "xz" >>= readIORef
    case mkFinGeneric word 1 of
        Nothing -> fail "missing position"
        Just pos -> do
            let selected = choose word pos True (error "forced explicitly lazy argument")
                fallback = choose word pos False '!'
            unless ((selected, fallback) == ('z', '!')) (fail "wrong branch")
    (ordinary, linear) <- observeCursors
    let expectedCursors = [Just 'c', Nothing, Nothing, Just 'b', Just 'x', Just 'y']
    unless (ordinary == expectedCursors && linear == expectedCursors) (fail (show (ordinary, linear)))
    left <- newIORef (error "skipped element" : 'L' : error "left tail") >>= readIORef
    right <- newIORef (13 : error "right tail") >>= readIORef
    let firstOnly = fmap fst (lookupBoth ('R' : error "left tail") 0
                             (error "unused selection" : right) 0)
    unless (lookupBoth left 1 right 0 == Just ('L', 13 :: Int) && firstOnly == Just 'R') $
        fail "wrong value or demand under a retained as-pattern"
    empty <- observeEmptyIndex
    gate <- newIORef (Shut (error "gate field")) >>= readIORef
    denied <- try (evaluate (grant gate (error "ticket"))) :: IO (Either ErrorCall Bool)
    case denied of
        Left err | empty && grant (Open True) Granted
                 , takeWhile (/= '\n') (displayException err) == "ticket" -> pure ()
        _ -> fail "wrong demand for an omitted retained equation"
    views <- observeConstructedViews
    unless (views == (Just 1, True)) (fail (show views))
    positions <- newIORef [minBound, -1, 0, 1, 2, 3, maxBound] >>= readIORef
    let cells = VCons (7 :: Int) (VCons 9 (VCons 13 VNil))
        other = VCons (11 :: Int) (VCons 19 (VCons 23 VNil))
        expected = [Nothing, Nothing, Just 7, Just 9, Just 13, Nothing, Nothing]
    case (map (lookupVecChecked cells) positions, map (lookupVecViaView cells) positions,
          fmap (\i -> indexVec i other) (mkFinVec cells 2)) of
        (direct, viaView, Just 23) | direct == expected && viaView == expected -> pure ()
        actual -> fail (show actual)
    let absent = error "forced vector for negative position" :: Vec Int 'Z
        unusedTail = error "forced suffix past checked position" :: Vec Int ('S 'Z)
        prefix = VCons (error "forced skipped element") (VCons (29 :: Int) unusedTail)
        selectedBottom = VCons (7 :: Int) (VCons (error "forced selected element") VNil)
    case (lookupVecViaView absent minBound, lookupVecViaView (VNil :: Vec Int 'Z) 0,
          lookupVecViaView prefix 1, isJust (lookupVecViaView selectedBottom 1)) of
        (Nothing, Nothing, Just 29, True) -> pure ()
        actual -> fail (show actual)
    case (lookupVecChecked absent minBound, lookupVecChecked (VNil :: Vec Int 'Z) 0,
          lookupVecChecked prefix 1, isJust (lookupVecChecked selectedBottom 1)) of
        (Nothing, Nothing, Just 29, True) -> pure ()
        actual -> fail (show actual)
    cut <- newIORef (S (S Z)) >>= readIORef
    let joined = appendNative @Int cut (S Z) (replicateNative cut 7) (VCons 9 VNil)
    case lookupVecChecked (takeVec @('S 'Z) cut joined) 1 of
        Just 7 -> pure ()
        actual -> fail (show actual)
    runResultClient
    runMultiIndex
    runConsProof
