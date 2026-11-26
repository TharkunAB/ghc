{-# LANGUAGE GADTs, NoDependentTypes, PolyKinds, RankNTypes, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy (Proxy))
import Data.Type.Equality ((:~:) (Refl))
import NativeTypeLambdaCaptureUProvider

eqToLeibnizU :: (x :~: y) -> LeibnizU x y
eqToLeibnizU Refl px = px

symViaLeibnizU :: (x :~: y) -> (y :~: x)
symViaLeibnizU xy = leibnizUToEq (leibnizUSym (eqToLeibnizU xy))

main :: IO ()
main = do
  let r1 = leibnizUSym @Int @Int (eqToLeibnizU Refl) @(ConstU Int) (42 :: Int)
      r2 = leibnizUSymSpec @String @String (eqToLeibnizU Refl) @(ConstU String) "ok"
      r3 = leibnizUTrans @Bool @Bool @Bool (eqToLeibnizU Refl) (eqToLeibnizU Refl) @(ConstU Bool) True
      Refl = symViaLeibnizU (Refl :: Int :~: Int)
      Ticket t1 = forwardPartial (Proxy @Char) (Ticket (Just (Right 7)) :: StampedTicket Char)
      Ticket t2 = forwardCaptureU (Proxy @(ConstU String)) (Ticket (Just "tag") :: ConstTicket String)
      w = matchableWrap (Just 'a')
  print (r1, r2, r3, t1, t2, w)
