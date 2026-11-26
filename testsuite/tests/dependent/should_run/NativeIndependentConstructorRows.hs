{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, TypeOperators #-}
module Main where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Type.Equality ((:~:)(Refl))
import qualified NativeIndependentConstructorRowsGuarded as G
import qualified NativeIndependentConstructorRowsIndep as I

nextIndep :: foreach (t :: I.Term) -> foreach (l :: I.Label) -> foreach (v :: I.Term)
          -> I.decorate ('I.Next t) l v :~: 'I.Next (I.decorate t l v)
nextIndep _ _ _ = Refl

nextGuarded :: foreach (t :: G.Term) -> foreach (l :: G.Label) -> foreach (v :: G.Term)
            -> G.decorate ('G.Next t) l v :~: 'G.Next (G.decorate t l v)
nextGuarded _ _ _ = Refl

knownGuarded :: foreach (v :: G.Term) -> G.decorate ('G.Atom 'G.Here) 'G.Here v :~: v
knownGuarded _ = Refl

givenGuarded :: foreach (x :: G.Label) -> foreach (l :: G.Label) -> foreach (v :: G.Term)
             -> G.matches x l :~: 'True -> G.decorate ('G.Atom x) l v :~: v
givenGuarded _ _ _ Refl = Refl

main :: IO ()
main = do
  case (nextIndep I.Empty I.Here I.Empty, nextGuarded G.Empty G.Here G.Empty, knownGuarded G.Empty) of
    (Refl, Refl, Refl) -> putStrLn "Refl"
  case I.decorate I.Empty (error "label") (error "replacement") of
    I.Empty -> pure ()
    _ -> error "expected Empty"
  case I.decorate (I.Next (error "field")) (error "label") (error "replacement") of
    I.Next _ -> pure ()
    _ -> error "expected Next"
  result <- try (evaluate (G.decorate (G.Atom (error "label")) G.Here G.Empty)) :: IO (Either ErrorCall G.Term)
  case result of
    Left _ -> putStrLn "strict guard"
    Right _ -> error "expected the guard to fail"
