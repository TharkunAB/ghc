{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeOrdinaryInlineProvider (advance, select) where

type family Payload (flag :: Bool) where
    Payload True = Int
    Payload False = Bool

{-# INLINABLE advance #-}
advance :: Int -> Int
advance n =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused _ = error "forced unused advance binding"
    in n + 1

{-# INLINE select #-}
select :: Bool -> Int -> Int
select flag =
    let unused :: foreach (flag :: Bool) -> Payload flag
        unused _ = error "forced unused select binding"
    in case flag of
        True -> (+ 1)
        False -> \_ -> 0
