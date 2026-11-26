{-# LANGUAGE DataKinds, DependentTypes, GADTs, TemplateHaskell, TypeOperators #-}
{-# OPTIONS_GHC -fomit-interface-pragmas -Wno-inline-rule-shadowing #-}
module NativeOpenMinimumDormantRule where

import Data.Type.Equality ((:~:)(Refl))

data Size = Zero | Succ Size

minSize :: Size -> Size -> Size
minSize Zero _ = Zero
minSize (Succ _) Zero = Zero
minSize (Succ m) (Succ n) = Succ (minSize m n)

zeroLawSame :: foreach (n :: Size) -> minSize 'Zero n :~: 'Zero
zeroLawSame _ = Refl

{-# RULES "minSize/dormantSame" [~] forall n. minSize Zero n = Succ Zero #-}

$(pure [])

minSizeLater :: Size -> Size -> Size
minSizeLater Zero _ = Zero
minSizeLater (Succ _) Zero = Zero
minSizeLater (Succ m) (Succ n) = Succ (minSizeLater m n)

zeroLawLater :: foreach (n :: Size) -> minSizeLater 'Zero n :~: 'Zero
zeroLawLater _ = Refl

$(pure [])

{-# RULES "minSize/dormantLater" [~] forall n. minSizeLater Zero n = Succ Zero #-}
