{-# LANGUAGE DataKinds, MagicHash, NoDependentTypes #-}
module NativeUnboxedCharPrimDisabled where
import Data.Proxy (Proxy)
import GHC.Exts (Char(C#))
type Bad = Proxy ('C# 'a'#)
