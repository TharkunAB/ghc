{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies, TypeOperators #-}
module NativeTypeLambdaQuoteHelpers (shadowed, wildcardEvidence) where

import Data.Type.Equality ((:~:)(Refl))
import Language.Haskell.TH

$(do s <- [t| \a -> \a -> a |]
     c <- [t| \a -> \b -> Either a b |]
     case (s, c) of
       (LamT [outer] (LamT [inner] (VarT use)),
        LamT [a] (LamT [b] (AppT (AppT (ConT _) (VarT a')) (VarT b'))))
         | outer /= inner, inner == use, a == a', b == b' -> pure []
       _ -> fail "A quoted lambda changed the scope of a binder")

$(do outer <- newName "label"
     inner <- newName "label"
     cell <- newName "cell"
     let top = mkName "shadowed"
         work = mkName "work"
         row = mkName "row"
         x = mkName "x"
         y = mkName "y"
         arrow arg result = AppT (AppT ArrowT arg) result
         triple a b c = AppT (AppT (AppT (TupleT 3) a) b) c
         quant name = ForallT [KindedTV name SpecifiedSpec StarT] []
         bool = ConT ''Bool
         topType = quant outer (arrow (VarT outer) (triple (VarT outer) (ConT ''Ordering) bool))
         workType = quant inner (arrow (VarT inner) (triple (VarT outer) (VarT inner) bool))
         callback = LamT [cell] (triple (VarT outer) (VarT (mkName "label")) (VarT cell))
         rowType = AppT callback bool
         rowValue = TupE (map Just [VarE x, VarE y, ConE 'True])
         rowDecls = [SigD row rowType, ValD (VarP row) (NormalB rowValue) []]
         workDecls = [SigD work workType, FunD work [Clause [VarP y] (NormalB (VarE row)) rowDecls]]
     pure [SigD top topType,
           FunD top [Clause [VarP x] (NormalB (AppE (VarE work) (ConE 'GT))) workDecls]])

$([d|
  type Direct = case 'Just 'True of { Nothing -> Char; Just _ -> Bool }
  type Nested = (\tag -> case tag of { Nothing -> Char; Just _ -> Bool }) ('Just 'True)
  type CatchAll = case ('Nothing :: Maybe Bool) of { _ -> Int }
  type family Family (tag :: Maybe Bool) where
    Family ('Just _) = Bool
    Family _ = Char
  |])

wildcardEvidence :: (Direct :~: Bool, Nested :~: Bool, CatchAll :~: Int, Family 'Nothing :~: Char)
wildcardEvidence = (Refl, Refl, Refl, Refl)
