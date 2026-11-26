{-# LANGUAGE DependentTypes, ListTuplePuns, Modifiers, ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures, TemplateHaskell, TypeApplications #-}
module NativeTypeLambdaTypedCallbacks (named, before, after, annotated, captured) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Matchability (U)
import Language.Haskell.TH.Syntax (Code, Q)

type WithError :: Type %U -> Type %U -> Type
type WithError = \err cell -> Either err cell

atInt :: forall (f :: Type %U -> Type). Proxy f -> f Int -> f Int
atInt _ x = x

named :: Code Q (Either String Int)
named = [|| atInt @(WithError String) Proxy (Right 42) ||]

before :: Code Q (Either String Int)
before = [|| atInt @(\cell -> Either String cell) Proxy (Right 42) ||]

$(pure [])

after :: Code Q (Either Char Int)
after = [|| atInt @((\err cell -> Either err cell) Char) Proxy (Left 'x') ||]

annotated :: Code Q (Either String Int)
annotated = [|| Right 43 :: (\err cell -> Either err cell) String Int ||]

captured :: Code Q (String -> Either String Int)
captured = [|| (\msg -> id @((\cell -> Either err cell) Int) (Left msg))
                :: forall err. err -> Either err Int ||]
