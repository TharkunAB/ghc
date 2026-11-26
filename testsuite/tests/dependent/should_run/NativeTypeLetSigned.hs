{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeTypeLetSigned (Braced, Layout, CaseChain, DependentCase) where
import Data.Kind (Type)
import Data.Proxy (Proxy)

type Braced (b :: Bool) = let { tag = Bool; tag :: tag; tag = b } in tag

type Layout (b :: Bool) =
  let tag = Bool
      tag :: tag
      tag = b
  in tag

type CaseChain (b :: Bool) =
  let first = case b of { True -> 'False; False -> 'True }
      second = case first of { True -> 'True; False -> 'False }
  in '(first, second)

type DependentCase (k :: Type) (x :: k) (flag :: Bool) (use :: Bool) =
  let k :: Type
      k = case flag of { True -> Bool; False -> Char }
      result :: Type
      result = case use of { True -> (Proxy x, Maybe k); False -> (Proxy x, Ordering) }
  in result
