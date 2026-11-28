{-# LANGUAGE DependentTypes, NoListTuplePuns #-}
{-# OPTIONS_GHC -Wall -Wpun-uses -Wunticked-promoted-constructors -Werror #-}
module NativeDataHeraldNoPunWarnings (boxDataT, boxDataList, boxDataPair, boxPunVar) where
import NativeDataHeraldInTypesProvider (Nat (..), PairBox (..), T (..))

boxDataT :: PairBox (data T)
boxDataT = MkPairBox

boxDataList :: PairBox (data [T])
boxDataList = MkPairBox

boxDataPair :: PairBox (data (T, True))
boxDataPair = MkPairBox

boxPunVar :: foreach (x :: Nat) -> PairBox (data x)
boxPunVar x = helper True
  where
    helper :: forall x. x -> PairBox (data x)
    helper _ = MkPairBox
