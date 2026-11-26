{-# LANGUAGE DataKinds, GADTs, NoDependentTypes, TypeOperators #-}
module Main where
import Control.Monad (unless)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(Refl))
import NativeTypeLambdaFormation (formed)
import NativeTypeLambdaOrdinary (capturedRow, independentRow, rankedSignature, rankedSynonym, scopedRow)
import SchemaColumns (Callable, Checked)
import TypeLambda (Columns, Fields(..), Optional, Prefixes, ScopedRow, withScopedErrors)
import TypeLambdaClient (forwardErrors)

withError :: Fields (Prefixes ('Just Ordering) '[Char, Bool])
withError = case capturedRow of Refl -> Field (Left GT) (Field (Right True) End)

withoutError :: Fields (Prefixes 'Nothing '[Char, Bool])
withoutError = case independentRow of Refl -> Field (Just 'a') (Field (Just True) End)

callable :: Fields (Columns (Callable String) '[Bool])
callable = Field wrap End
  where
    wrap :: Fields '[Bool] -> Fields '[Either String Bool]
    wrap (Field x End) = Field (Right x) End

main :: IO ()
main = case (capturedRow, independentRow, withError, withoutError) of
  (Refl, Refl, Field a (Field b End), Field c (Field d End)) -> do
    unless (formed Proxy) $ fail "local type lambda formation"
    print (a, b, c, d)
    let row :: Fields (ScopedRow Char '[Bool])
        row = case scopedRow of Refl -> Field (Just 'x', Right True) End
    case row of Field value End -> print value
    let input = Field 'x' (Field (error "unused field" :: Bool) End)
    case forwardErrors (Proxy :: Proxy String) input of
      Field x (Field _ End) -> print x
    case withScopedErrors (Proxy :: Proxy Ordering) input of
      Field x (Field _ End) -> print x
    let optional :: Fields (Columns Optional '[Int])
        optional = Field (Just 42) End
        checked :: Fields (Columns (Checked String) '[Bool])
        checked = Field (Left "missing") End
    case (optional, checked) of
      (Field x End, Field y End) -> print (x, y)
    case callable of
      Field wrap End -> case wrap (Field True End) of
        Field x End -> print x
    case (rankedSynonym (Proxy :: Proxy 'False), rankedSignature (Proxy :: Proxy 'True)) of
      (Field (_, _, x) End, Field (_, _, y) End) -> print (x, y)
