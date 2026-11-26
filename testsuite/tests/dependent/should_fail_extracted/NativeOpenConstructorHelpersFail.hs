{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             ScopedTypeVariables, TypeOperators #-}
module NativeOpenConstructorHelpersFail where

import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeOpenConstructorHelpersResidual (Erase, Second)
import NativeOpenIndexedSubstSyntax (Ctx(..), Name, SomeTerm, Sort(..), Term)
import qualified NativeOpenIndexedSubstSyntax as S
import qualified SpineWorker as W

unknownTail :: forall (a :: Bool) (xs :: W.Spine Bool).
               Proxy a -> Proxy xs -> Second ('W.More a xs) :~: 'Just a
unknownTail _ _ = Refl

erasedTail :: forall (x :: Bool) (xs :: W.Spine Bool).
              Proxy x -> Proxy xs -> Erase ('W.More x xs) :~: 'W.More 'True 'W.Empty
erasedTail _ _ = Refl

wrongSubst :: forall (g :: Ctx) (x :: Name g 'Num).
              S.substOne
                ('S.Bind ('S.Use ('S.There 'S.Here))
                  :: Term ('Extend g 'Num) ('Fun 'Num 'Num))
                ('S.Use x)
              :~: 'S.Bind ('S.Use 'S.Here)
wrongSubst = Refl

wrongExistential :: forall (x :: Term ('Extend 'Empty 'Num) 'Num)
                           (y :: Term ('Extend 'Empty 'Num) 'Num).
                    S.packTerm ('S.Here :: Name ('Extend 'Empty 'Num) 'Num) x
                    :~: 'S.Pack 'Refl y
wrongExistential = Refl

wrongUnary :: forall (x :: Term ('Extend 'Empty 'Num) 'Num)
                     (y :: Term ('Extend 'Empty 'Num) 'Num).
              S.unpackOne ('S.Pack 'Refl x :: SomeTerm ('Extend 'Empty 'Num) 'Num) :~: y
wrongUnary = Refl
