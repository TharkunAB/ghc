{-# LANGUAGE DataKinds, NoDependentTypes, TypeOperators #-}
module NativeTypeLetOrdinary where
import Data.Type.Equality ((:~:)(Refl))
import Data.Proxy (Proxy(..))
import NativeTypeLetPairBindings (AliasCount, Held, KnownPrefix, ShadowCount,
                     NestedCount, NestedRight, NestedHeight,
                     Axis(..), AxisLength, Volume(..), VolumeRows, VolumeFields,
                     Grid(..), GridTotal, GridRows, GridHeld, OpaqueGrid, OpaqueColumn)
import qualified NativeTypeLetSignature as SignatureLet
import NativeTypeLetSigned (Braced, Layout, CaseChain, DependentCase)
import TypeLet

roadmap :: F Bool :~: Ordering
roadmap = Refl

shadow :: Shadow 'False :~: Ordering
shadow = Refl

outer :: Outer 'False :~: Char
outer = Refl

reuse :: Reuse 'True :~: '( 'True, 'True)
reuse = Refl

nested :: Nested 'False :~: '( 'False, 'False)
nested = Refl

capture :: CaptureBeforeShadow 'False :~: '( 'False, 'True)
capture = Refl

forward :: a -> Bool -> a
forward = SignatureLet.capture

signatureCase :: Proxy Bool
signatureCase = SignatureLet.selected

braced :: Braced 'True :~: 'True
braced = Refl

layout :: Layout 'False :~: 'False
layout = Refl

caseChain :: CaseChain 'True :~: '( 'False, 'False)
caseChain = Refl

dependentCapture :: DependentCase (Maybe Bool) ('Just 'True) 'True 'True
                 :~: (Proxy ('Just 'True), Maybe Bool)
dependentCapture = Refl

dependentUnused :: Proxy flag -> DependentCase (Maybe Bool) ('Just 'True) flag 'False
                :~: (Proxy ('Just 'True), Ordering)
dependentUnused _ = Refl

knownPair :: DimensionSum '(2, 3) :~: 5
knownPair = Refl

chosenWidth :: DimensionWithOverride 'False 'Nothing :~: 2
chosenWidth = Refl

inlineDefaultWidth :: Proxy 3
inlineDefaultWidth = defaultWidth

explicitWidth :: Proxy width -> DimensionWithOverride width ('Just 7) :~: 7
explicitWidth _ = Refl

knownProduct :: GridTotal ('Grid 2 3) :~: 5
knownProduct = Refl

oneField :: AxisLength ('Axis 2) :~: 2
oneField = Refl

threeFields :: VolumeRows ('Volume 2 'False OpaqueColumn) :~: 2
threeFields = Refl

allFields :: VolumeFields ('Volume 2 'True 4) :~: '(2, 'True, 4)
allFields = Refl

lazyProduct :: GridRows ('Grid 2 OpaqueColumn) :~: 2
lazyProduct = Refl

unknownProduct :: Proxy (GridRows OpaqueGrid)
unknownProduct = Proxy

unusedProduct :: GridHeld :~: Bool
unusedProduct = Refl

nestedLeftPair :: NestedCount 7 :~: 10
nestedLeftPair = Refl

nestedRightPair :: NestedRight 3 :~: 5
nestedRightPair = Refl

nestedUnusedPair :: Proxy n -> NestedHeight n :~: 3
nestedUnusedPair _ = Refl

aliasPair :: AliasCount :~: 5
aliasPair = Refl

opaqueField :: Proxy n -> KnownPrefix n :~: 2
opaqueField _ = Refl

heldPair :: Proxy dimensions -> Held dimensions :~: Bool
heldPair _ = Refl

shadowPair :: ShadowCount 2 :~: 5
shadowPair = Refl

yes :: CaseResult 'True :~: Either Bool Bool
yes = Refl

no :: CaseResult 'False :~: Either Char Char
no = Refl
