{-# LANGUAGE Haskell2010, DataKinds #-}
module NativeDataHeraldNoExplicitNsTy where
import Data.Proxy (Proxy (Proxy))
bad :: Proxy (data True)
bad = Proxy
