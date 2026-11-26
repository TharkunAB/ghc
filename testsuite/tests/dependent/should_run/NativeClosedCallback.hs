{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeOperators #-}
module Main (main) where

import Data.Type.Equality ((:~:)(Refl))
import qualified NativeClosedCallbackForward as C
import qualified NativeClosedCallbackLib as W
import qualified Prelude as P

prelude :: P.map P.not '[ 'P.True, 'P.False] :~: '[ 'P.False, 'P.True]
prelude = Refl

complemented :: W.each (W.flipBool) '[ 'P.True, 'P.False] :~: '[ 'P.False, 'P.True]
complemented = Refl

constant :: W.each W.setTrue '[ 'P.True, 'P.False] :~: '[ 'P.True, 'P.True]
constant = Refl

polymorphic :: W.each P.id '[ 'P.True, 'P.False] :~: '[ 'P.True, 'P.False]
polymorphic = Refl

forwarded :: W.each C.step '[ 'P.True] :~: '[ 'P.False]
forwarded = Refl

main :: P.IO ()
main = forwarded `P.seq` P.print (prelude, complemented, constant, polymorphic)
