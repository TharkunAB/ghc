{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, Modifiers,
             RankNTypes, RequiredTypeArguments, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-enable-rewrite-rules #-}
module NativeOpenDropWhileSpanClient
  ( closedSpanOk, shapeProofOk, deciderUnambiguous ) where

import Data.Kind (Type)
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (U)
import qualified NativeOpenDropWhileWalkShapes as W
import NativeOpenSpanShapes (lazySpan)
import PreludeDropWhileSource (Rejoined)
import PreludeFilterAppendProof (FilterDecisions(..))
import PreludeFilterRuntime (BooleanResult(..), negationResult)
import PreludeFilterSource (Negation)

type ShapeSpan :: (a %U -> Bool) -> [a] -> ([a], [a])
type ShapeSpan p xs = lazySpan p xs

type ShapePrefix :: (a %U -> Bool) -> [a] -> [a]
type ShapePrefix p xs = fst (ShapeSpan p xs)

type ShapeSuffix :: (a %U -> Bool) -> [a] -> [a]
type ShapeSuffix p xs = snd (ShapeSpan p xs)

spanFirst :: Proxy xs -> ShapePrefix Negation ('False ': xs) :~: ('False ': ShapePrefix Negation xs)
spanFirst _ = Refl

spanSecond :: Proxy xs -> ShapeSuffix Negation ('False ': xs) :~: ShapeSuffix Negation xs
spanSecond _ = Refl

spanReject :: Proxy xs -> ShapeSuffix Negation ('True ': xs) :~: ('True ': xs)
spanReject _ = Refl

rejoinShape :: FilterDecisions p xs -> Rejoined (ShapeSpan p xs) :~: xs
rejoinShape Done = Refl
rejoinShape (Decide IsFalse _) = Refl
rejoinShape (Decide IsTrue ds) = case rejoinShape ds of
  Refl -> Refl

walkProofs :: Proxy xs ->
  ( W.walkDrop Negation ('False ': xs) :~: W.walkDrop Negation xs
  , W.walkKeep Negation ('False ': xs) :~: ('False ': W.walkKeep Negation xs)
  , W.walkStop Negation ('True ': xs) :~: '[] )
walkProofs _ = (Refl, Refl, Refl)

type ClosedSplit = span not '[ 'False, 'True, 'False]

closedPieces :: ClosedSplit :~: '( '[ 'False], '[ 'True, 'False])
closedPieces = Refl

closedReassembled :: Rejoined ClosedSplit :~: '[ 'False, 'True, 'False]
closedReassembled = Refl

closedSpanOk :: Bool
closedSpanOk = case (closedPieces, closedReassembled) of (Refl, Refl) -> True

shapeProofOk :: Bool
shapeProofOk =
  let ds :: FilterDecisions Negation '[ 'False, 'True]
      ds = Decide IsTrue (Decide IsFalse (error "unused"))
      _k = (spanFirst, spanSecond, spanReject, walkProofs)
  in case rejoinShape ds of Refl -> True

choose :: forall (a :: Type) b. forall (p :: a %U -> Bool) -> Show b
       => (foreach (x :: a) -> BooleanResult (p x)) -> Int
choose (type _) _ = 0

deciderUnambiguous :: Int
deciderUnambiguous = choose @Bool @Int (type Negation) negationResult
