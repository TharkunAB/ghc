{-# LANGUAGE DataKinds, DependentTypes, GADTs, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -O2 -fenable-rewrite-rules -Wno-inline-rule-shadowing #-}
module NativeOpenMinimumRuleFail where

import Data.Type.Equality ((:~:)(Refl))

data Size = Zero | Succ Size

minSizeSame :: Size -> Size -> Size
minSizeSame Zero _ = Zero
minSizeSame (Succ _) Zero = Zero
minSizeSame (Succ m) (Succ n) = Succ (minSizeSame m n)

zeroLawSame :: foreach (n :: Size) -> minSizeSame 'Zero n :~: 'Zero
zeroLawSame _ = Refl

{-# RULES "minSizeSame/zero" forall n. minSizeSame Zero n = Succ Zero #-}

minSizeLater :: Size -> Size -> Size
minSizeLater Zero _ = Zero
minSizeLater (Succ _) Zero = Zero
minSizeLater (Succ m) (Succ n) = Succ (minSizeLater m n)

zeroLawLater :: foreach (n :: Size) -> minSizeLater 'Zero n :~: 'Zero
zeroLawLater _ = Refl

$(pure [])

{-# RULES "minSizeLater/zero" forall n. minSizeLater Zero n = Succ Zero #-}
