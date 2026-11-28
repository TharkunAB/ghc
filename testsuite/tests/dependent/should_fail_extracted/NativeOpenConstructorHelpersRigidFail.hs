{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, GADTs, PolyKinds,
             ScopedTypeVariables, TypeOperators #-}
module NativeOpenConstructorHelpersRigidFail where

import Data.Type.Equality ((:~:)(Refl))
import NativeOpenIndexedSubstSyntax (Ctx(..), Sort(..), Term)
import qualified NativeOpenIndexedSubstSyntax as S

rigidSubst :: forall (g :: Ctx) (x :: Term g 'Num).
              S.substOne
                ('S.Bind ('S.Use ('S.There 'S.Here))
                  :: Term ('Extend g 'Num) ('Fun 'Num 'Num))
                x
              :~: 'S.Bind ('S.Use 'S.Here)
rigidSubst = Refl
