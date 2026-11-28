{-# LANGUAGE DataKinds, ExplicitNamespaces, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:)(Refl))
import NativeLargeElimTypeProvider
  ( BoxVal(..), Val(..), idArr, tupleOf, valPred, valRel )
import qualified NativeLargeElimIndexedCli as ICli
import qualified NativeLargeElimIndexedSame as ISame

proofBot :: valPred 'Bot :~: ()
proofBot = Refl

proofArr :: valPred ('Arr 'Bot ('Arr 'Bot 'Bot)) :~: (() -> () -> ())
proofArr = Refl

proofRel :: valRel ('Arr 'Bot 'Bot) ('Arr 'Bot 'Bot) :~: ((() -> ()) -> (() -> ()))
proofRel = Refl

proofTuple :: tupleOf '[ '(), '() ] :~: ((), ((), ()))
proofTuple = Refl

defaultVal :: foreach (v :: Val) -> valPred v
defaultVal Bot       = ()
defaultVal (Arr _ b) = \_ -> defaultVal b

evalVal :: foreach (v :: Val) -> valPred v -> ()
evalVal Bot       x = x
evalVal (Arr a b) f = evalVal b (f (defaultVal a))

unboxEval :: foreach (v :: Val) -> BoxVal v -> ()
unboxEval v (MkBoxVal payload) = evalVal v payload

main :: IO ()
main = ICli.useClosJoin `seq` ISame.sameBot `seq` do
  let r1 = case (proofBot, proofArr, proofRel, proofTuple) of
             (Refl, Refl, Refl, Refl) -> True
      r2 = idArr () == ()
      r3 = evalVal (Arr (Arr Bot Bot) (Arr Bot Bot)) (\g x -> g x) == ()
      r4 = unboxEval (Arr Bot Bot) (MkBoxVal idArr) == ()
      tup :: tupleOf '[ 'True, 'False ]
      tup = (True, (False, ()))
      r5 = fst tup && not (fst (snd tup))
  print (r1, r2, r3, r4, r5)
