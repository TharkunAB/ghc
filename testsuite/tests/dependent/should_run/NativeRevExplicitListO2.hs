{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             RankNTypes, TypeApplications, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall(..), evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import GHC.Exts (build)
import NativeRevExplicitListO2BoxMod ()
import NativeRevExplicitListO2MapGuard (checkMapAppend)
import NativeRevExplicitListO2Ops (app, rev)
import NativeRevExplicitListO2RetainedOps (appRet, revRet)
import NativeRevExplicitListO2Shunt (shunt, shuntAppend)

revNil :: forall a. rev ('[] :: [a]) :~: '[]
revNil = Refl

revStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
        -> rev (x ': xs) :~: app (rev xs) '[x]
revStep _ _ = Refl

revRetStep :: foreach (x :: Bool) -> foreach (xs :: [Bool])
           -> revRet (x ': xs) :~: appRet (revRet xs) '[x]
revRetStep _ _ = Refl

checkShunt :: [Bool] -> [Bool] -> [Bool] -> Bool
checkShunt xs ys zs = case shuntAppend xs ys zs of
  Refl -> shunt (xs ++ ys) zs == shunt ys (shunt xs zs)

{-# INLINE producer #-}
producer :: (Bool -> b -> b) -> b -> b
producer cons seed = seed `seq` cons True seed

{-# NOINLINE observeFusedLHS #-}
observeFusedLHS :: [Bool] -> Bool
observeFusedLHS ys = null (map not (build producer ++ ys))

{-# NOINLINE observeSeparateRHS #-}
observeSeparateRHS :: [Bool] -> (Bool, Bool)
observeSeparateRHS ys =
  let mxs = map not (build producer)
  in (null mxs, null (mxs ++ map not ys))

main :: IO ()
main = do
  print (revNil @Bool, revStep True [False], rev [True, False])
  case revRetStep True [] of
    Refl | checkMapAppend -> pure ()
         | otherwise -> error "checkMapAppend failed"
  if not (checkShunt [True, False] [False] [True]) || build producer /= [True]
    then error "unexpected shunt or producer"
    else pure ()
  lhs <- try (evaluate (observeFusedLHS (error "ys"))) :: IO (Either ErrorCall Bool)
  rhs <- evaluate (observeSeparateRHS (error "ys"))
  case (lhs, rhs) of
    (Left _, (False, False)) -> putStrLn "fusion counterexample verified"
    other -> error ("Unexpected outcome: " ++ show other)
