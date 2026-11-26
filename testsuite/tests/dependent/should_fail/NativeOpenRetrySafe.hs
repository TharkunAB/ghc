{-# LANGUAGE Haskell2010, Safe #-}
{-# LANGUAGE DataKinds, DependentTypes, NamedWildCards #-}
{-# LANGUAGE PartialTypeSignatures, ScopedTypeVariables, TypeApplications #-}
module NativeOpenRetrySafe where
import qualified RetryCharacters
import Data.Proxy (Proxy(..))
upper :: Proxy _c -> Proxy (RetryCharacters.title _c)
upper (_ :: Proxy 'a') = Proxy @'A'
