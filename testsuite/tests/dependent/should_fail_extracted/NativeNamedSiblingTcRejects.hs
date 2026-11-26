{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing -Wno-orphans #-}
module NativeNamedSiblingTcRejects where

import NamedSiblingColumns (NamedColumns(..))
import qualified NamedSiblingColumns as NS
import PackedVecLibrary (Nat(..), Vec(..), previous, replicateNative, toList, zipVec)
import qualified ParamColumns as PC

data Plain = Plain { counted :: Nat }

badOrdinarySel :: Plain -> [Char]
badOrdinarySel p = toList (replicateNative (counted p) '-')

badShadowSel :: NamedColumns -> [Char]
badShadowSel p = let storedLength _ = Z in toList (replicateNative (storedLength p) '-')

{-# RULES "previous/reset" forall n. previous n = Z #-}

badRuleSel :: NamedColumns -> [Char]
badRuleSel p = toList (replicateNative (previous (NS.storedLength p)) '-')

badParamReceiver :: foreach (p :: PC.Columns a) (q :: PC.Columns a) -> Vec a (PC.storedLength p)
badParamReceiver _ q = PC.cells q

missingWitness :: NamedColumns
missingWitness = NamedColumns { namedLetters = VNil, namedFlags = VNil }

data OrdinaryStrict = OrdinaryStrict { strictField :: !Bool, lazyField :: Int }

missingStrict :: OrdinaryStrict
missingStrict = OrdinaryStrict { lazyField = 0 }

badConstruct :: NamedColumns
badConstruct = NamedColumns { NS.storedLength = S Z, namedLetters = VNil, namedFlags = VNil }

badDotSelection :: NamedColumns -> Vec Bool 'Z
badDotSelection p = p.namedFlags

badUpdateOnlyWitness :: NamedColumns -> NamedColumns
badUpdateOnlyWitness p = p { NS.storedLength = Z }

missingChangedPayload :: NamedColumns
missingChangedPayload =
  let old = NamedColumns (S Z) (VCons 'x' VNil) (VCons True VNil)
  in old { NS.storedLength = Z, namedFlags = VNil }

otherWitness :: NamedColumns -> NamedColumns -> NamedColumns
otherWitness p q = p { NS.storedLength = NS.storedLength q, namedFlags = namedFlags q }

shadowWitness :: NamedColumns -> NamedColumns
shadowWitness p =
  let storedLength _ = Z
  in p { NS.storedLength = storedLength p, namedFlags = VNil }

wrongNewPayload :: NamedColumns -> NamedColumns
wrongNewPayload p = p { NS.storedLength = Z, namedLetters = VCons 'x' VNil, namedFlags = VNil }

badPayloadUpdate :: NamedColumns -> NamedColumns -> NamedColumns
badPayloadUpdate p q = p { namedFlags = namedFlags q }

badSiblingUpdate :: NamedColumns -> NamedColumns -> NamedColumns
badSiblingUpdate p q = p { namedLetters = namedLetters p, namedFlags = namedFlags q }

badZip :: NamedColumns -> NamedColumns -> [(Char, Bool)]
badZip p q = toList (zipVec (namedLetters p) (namedFlags q))
