{-# LANGUAGE DataKinds, FlexibleContexts, GADTs, ImpredicativeTypes, NoDependentTypes #-}
{-# LANGUAGE TypeFamilies, TypeOperators #-}
module Main (main) where

import Control.Exception (ErrorCall, evaluate, try)
import Data.Proxy (Proxy (..))
import Data.Type.Equality ((:~:) (Refl))
import GHC.Sigma (Sigma (Pair), first)
import qualified NativeAssociatedSumClass as Assoc
import NativeAssociatedSumProvider (DefaultSum (Defaulted), Tagged (..))
import NativeConstrainedSumProvider
  ( Allowed
  , Choice (Shape)
  , Forward (Forwarded)
  , Permit (Permit)
  , Staged (Staged)
  , make
  )

instance DefaultSum Char

readAssociated :: Assoc.Result [Int] Char -> Bool
readAssociated (Pair tag _) = tag

readAssocKind :: Assoc.Keep (Proxy False) Bool True -> Bool
readAssocKind (Pair tag _) = tag

readFirst :: Defaulted Char -> Bool
readFirst = first

readPair :: Defaulted Char -> Bool
readPair pkg = case pkg of Pair tag _payload -> tag

readPayload :: Defaulted Char -> (Bool, Char)
readPayload pkg = case pkg of
  Pair tag payload -> (tag, case payload of Yes c -> c; No c -> c)

readGiven :: Permit a -> Proxy a -> Bool
readGiven Permit ix = case make ix of Pair tag _ -> tag

readKnown :: Shape Bool Char -> Bool
readKnown pkg = case pkg of Pair tag _ -> tag

readRefined :: (a ~ Bool) => Shape a Char -> Bool
readRefined pkg = case pkg of Pair tag _ -> tag

readProof :: Choice a => a :~: Bool -> Shape a Char -> Bool
readProof Refl pkg = case pkg of Pair tag _ -> tag

readStaged :: Staged a -> Bool
readStaged (Staged _ pkg) = case pkg of Pair tag _ -> tag

readDefaultPair :: Bool -> Bool
readDefaultPair tag = readPair (Pair tag (error "unused constructed default payload"))

readForwarded :: Allowed a => Proxy a -> Forwarded [a] -> Bool
readForwarded _ = first

readGivenPair :: Permit a -> Proxy a -> Bool -> Bool
readGivenPair Permit ix tag = readForwarded ix (Pair tag (error "unused constructed forwarded payload"))

main :: IO ()
main = do
  outer <- try (evaluate (readPair (error "outer pair"))) :: IO (Either ErrorCall Bool)
  assocOuter <- try (evaluate (readAssociated (error "outer pair"))) :: IO (Either ErrorCall Bool)
  case (outer, assocOuter) of
    (Left _, Left _) -> do
      if readDefaultPair False || not (readGivenPair Permit (Proxy :: Proxy Bool) True)
        then fail "incorrect witness from a constructed package"
        else pure ()
      let pkg = Pair False (error "unused default payload") :: Defaulted Char
          shaped = Pair True (error "unused shaped payload") :: Shape Bool Char
          staged = Staged (error "unused earlier field" :: Proxy Bool)
                          (Pair True (error "unused later payload"))
      print (readFirst pkg, readPair pkg, readPayload (Pair True (Yes 'x')),
             readGiven Permit (Proxy :: Proxy Bool), readKnown shaped,
             readRefined shaped, readProof Refl shaped, readStaged staged)
      print (readAssociated (Pair True (error "unused instance payload")),
             readAssocKind (Pair False (Proxy :: Proxy False, Proxy :: Proxy False, Proxy :: Proxy True)),
             readFirst (Pair True (Yes 'q')),
             readFirst (Pair False (error "unused default payload")))
    _ -> fail "package pattern left the outer constructor unevaluated"
