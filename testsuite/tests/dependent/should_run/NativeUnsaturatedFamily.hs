{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeApplications, TypeOperators #-}
module Main (main) where

import Data.Proxy (Proxy(Proxy))
import Data.Type.Equality ((:~:)(Refl))
import NativeBuiltinTypeFunctions (Added, Compared, Prefixed, added, compared, prefixed)
import NativePartialSynonym (Saved, atChar, forwarded, forwardedVariable)
import qualified NativeStandaloneUnsaturationClient as S
import NativeStandaloneUnsaturationProvider (Box(Box))
import qualified NativeUnsaturatedClient as C
import NativeUnsaturatedEta (answer)
import NativeUnsaturatedPattern (local, matchable, tag)
import qualified NativeUnsaturatedPattern as PM
import NativeUnsaturatedReexport (directChecked, noPolyProofs, rawProof)

useSaved :: Saved Bool -> Saved Bool
useSaved = id

builtinChecked :: (Added :~: '[3, 4, 5], Compared :~: '[ 'GT, 'EQ, 'LT ], Prefixed :~: '["ghc-one", "ghc-two"])
builtinChecked = case (added, compared, prefixed) of
  (Refl, Refl, Refl) -> (Refl, Refl, Refl)

main :: IO ()
main = do
  case ( matchable, rawProof, directChecked, fst noPolyProofs, snd noPolyProofs
       , C.reflexive, C.saturated, C.forwardedFamily, C.forwardedConstructor
       , C.writtenConstructor, C.namedFamily, C.namedConstructor
       , atChar, forwarded, forwardedVariable
       ) of
    (Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl, Refl) -> pure ()
  case builtinChecked of
    (Refl, Refl, Refl) -> pure ()
  let Box b1 = S.synonymBox
      Box b2 = S.boxedFamily
  useSaved `seq` print
    ( answer
    , (local, tag (Proxy @'False) (Proxy @(PM.Box 'False Maybe)) (Proxy @(Maybe Bool)))
    , (S.ordinary, S.familyVal, b1 == Just True && b2 && S.saturatedAlias)
    )
