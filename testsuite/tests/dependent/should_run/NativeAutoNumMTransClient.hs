{-# LANGUAGE DataKinds, GADTs, KindSignatures, PolyKinds #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module NativeAutoNumMTransClient (UseTag(..), useIdEndo, mtransOk) where
import NativeAutoNumMTransProvider (Endo, MTrans, idEndo)
newtype UseTag :: MTrans where
  MkUseTag :: m a -> UseTag m a
useIdEndo :: Endo
useIdEndo = idEndo
mtransOk :: ()
mtransOk = useIdEndo `seq` ()
