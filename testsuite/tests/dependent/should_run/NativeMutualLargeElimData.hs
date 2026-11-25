{-# LANGUAGE AllowAmbiguousTypes, DataKinds, DependentTypes, ExplicitNamespaces, GADTs, ListTuplePuns, TypeOperators #-}
module Main where

import Data.Type.Equality ((:~:) (Refl))
import NativeMutualLargeElimDataProvider

proofBot :: logVLam Bot :~: ()
proofBot = Refl

proofArr :: logVLam (Arr Bot Bot) :~: LogArr Bot Bot
proofArr = Refl

proofJoin :: logVLam (Join Bot (Arr Bot Bot)) :~: ((), LogArr Bot Bot)
proofJoin = Refl

proofIxBot :: logVLamIx Bot ENil (UVar (FZ @Zero)) :~: ()
proofIxBot = Refl

proofIxArr :: logVLamIx (Arr Bot Bot) ENil (UVar (FZ @Zero))
          :~: LogArrIx Bot Bot ENil (UVar (FZ @Zero))
proofIxArr = Refl

idHalts :: HaltsV (Arr Bot Bot)
idHalts = MkHaltsV (MkLogArr (\(MkHaltsV ()) -> MkHaltsV ()))

stepHalts :: HaltsV (Join Bot (Arr Bot Bot)) -> Bool
stepHalts (MkHaltsV ((), arr)) =
  case runLogArr arr (MkHaltsV ()) of
    MkHaltsV () -> True

runIx :: Bool
runIx =
  let leId :: LogEIx (Arr Bot (Arr Bot Bot)) ENil (ULam (ULam (UVar (FS (FZ @Zero)))))
      leId AboveArr =
        MkHaltsIx ELam
          (MkLogArrIx (\_ AboveArr -> MkHaltsIx ELam (MkLogArrIx (\_ af -> case af of {}))))
      leArg :: LogEIx Bot ENil (ULam (UVar (FZ @Zero)))
      leArg af = case af of {}
  in case fundamentalApp leId leArg AboveArr of
       MkHaltsIx (EApp ELam ELam) _ -> True

main :: IO ()
main = do
  let r1 = case (proofBot, proofArr, proofJoin, proofIxBot, proofIxArr) of
             (Refl, Refl, Refl, Refl, Refl) -> True
      r2 = botProof == ()
      r3 = case idHalts of
             MkHaltsV f -> case runLogArr f (MkHaltsV ()) of
                             MkHaltsV () -> True
      r4 = stepHalts (MkHaltsV ((), MkLogArr id))
      r5 = runIx
  print (r1, r2, r3, r4, r5)
