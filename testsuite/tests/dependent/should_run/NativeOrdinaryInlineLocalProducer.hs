{-# LANGUAGE BangPatterns, DependentTypes, ListTuplePuns #-}
module NativeOrdinaryInlineLocalProducer (inlineCapture, inlinableCapture, forwardLocal, nested, erasedCapture) where
import Data.Proxy (Proxy(..))

data Key (tag :: Bool) = Key

{-# NOINLINE choose #-}
choose :: foreach (tag :: Bool) -> Key tag -> Int -> Int -> Int
choose False _ _ n = n
choose True _ !capture n = capture + n

{-# NOINLINE inlineCapture #-}
inlineCapture :: Int -> Int -> Int
inlineCapture free =
    let {-# INLINE [0] useInline #-}
        useInline :: Int -> Int
        useInline n = choose True Key free n
    in useInline

{-# NOINLINE inlinableCapture #-}
inlinableCapture :: Int -> Int -> Int
inlinableCapture free =
    let {-# INLINABLE useInlinable #-}
        useInlinable :: Int -> Int
        useInlinable n = choose True Key free n
    in useInlinable

{-# NOINLINE withIndex #-}
withIndex :: foreach (tag :: Bool) -> Key tag -> Int -> Int -> Int
withIndex tag key free =
    let {-# INLINE useIndex #-}
        useIndex :: Int -> Int
        useIndex n = choose tag key free n
    in useIndex

{-# NOINLINE forwardLocal #-}
forwardLocal :: Int -> Int -> Int
forwardLocal = withIndex False Key

{-# INLINE [1] nested #-}
nested :: Int -> Int -> Int
nested free n =
    let {-# INLINE [0] inNested #-}
        inNested :: Int -> Int
        inNested x = choose True Key free x
    in inNested n + inNested n

{-# NOINLINE fromType #-}
fromType :: forall a. Proxy a -> Int -> Int
fromType _ n = n

{-# NOINLINE erasedCapture #-}
erasedCapture :: forall a. Proxy a -> (Int, Int)
erasedCapture =
    let {-# INLINABLE useErased #-}
        useErased :: Int
        useErased = choose True Key (fromType (Proxy @a) 17) 1
    in \_ -> (useErased, useErased)
