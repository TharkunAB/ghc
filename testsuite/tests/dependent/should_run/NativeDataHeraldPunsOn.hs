{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module NativeDataHeraldPunsOn (checkPunsOn) where
import Data.Proxy (Proxy)
import Data.Type.Equality ((:~:)(Refl))
import NativeDataHeraldInTypesProvider (T (..))

pNil :: Proxy (data ([] :: [T])) :~: Proxy ('[] :: [T])
pNil = Refl

pOne :: Proxy (data [T]) :~: Proxy '[ 'T ]
pOne = Refl

pUnit :: Proxy (data ()) :~: Proxy '()
pUnit = Refl

pPair :: Proxy (data (T, True)) :~: Proxy '( 'T, 'True )
pPair = Refl

pPairCon :: Proxy ((data (,)) (data T) (data False)) :~: Proxy '( 'T, 'False )
pPairCon = Refl

checkPunsOn :: Bool
checkPunsOn = seq pNil $ seq pOne $ seq pUnit $ seq pPair $ seq pPairCon True
