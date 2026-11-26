{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeOrdinaryInlinePackages
  ( packedLocal, openedLocal, unopenedLocal
  , packedExported, openedExported
  , topRegular, topBreaker, openedRec
  ) where

import Data.Kind (Type)

type HiddenMaybe = exists (a :: Type). (Maybe a, a -> Int)

{-# NOINLINE inspect #-}
inspect :: HiddenMaybe -> Int
inspect (value, render) = maybe 0 render value

{-# NOINLINE hideMaybe #-}
hideMaybe :: a -> (a -> Int) -> HiddenMaybe
hideMaybe value render = (Just value, render)

packedLocal :: Int -> Bool -> Int -> Int
packedLocal offset keep n =
    let {-# INLINE packLocal #-}
        packLocal :: Int -> Int
        packLocal x = inspect ((if keep then Just x else Nothing, (+ offset)) :: HiddenMaybe)
    in packLocal n + 1

openedLocal :: Int -> Int -> Int
openedLocal offset n =
    let {-# INLINABLE openLocal #-}
        openLocal :: Int -> Int
        openLocal x = case hideMaybe x (+ offset) of
            (value, render) -> maybe 0 render value
    in openLocal n + openLocal 2

unopenedLocal :: Int -> Int
unopenedLocal offset =
    let {-# INLINE unusedLocal #-}
        unusedLocal :: Int -> Int
        unusedLocal _ = case (error "forced unused package" :: HiddenMaybe) of
            (value, render) -> maybe 0 render value
    in offset

type Hidden = exists (a :: Type). (a, a -> Int)

{-# NOINLINE consume #-}
consume :: Hidden -> Int
consume (value, render) = render value

{-# INLINE packedExported #-}
packedExported :: Int -> Bool -> Int -> Int
packedExported offset keep n =
    consume ((n, \v -> if keep then offset + v else 0) :: Hidden)

{-# NOINLINE hide #-}
hide :: Int -> Hidden
hide n = (n, (+ 1))

{-# INLINABLE openedExported #-}
openedExported :: Int -> Int
openedExported n = case hide n of
    (value, render) -> render value

{-# INLINE topRegular #-}
topRegular :: Int -> Bool -> Int -> Int
topRegular steps keep n =
    if steps <= 0
      then consume ((n, \v -> if keep then v + 1 else 0) :: Hidden)
      else peerRegular (steps - 1) keep n

{-# NOINLINE peerRegular #-}
peerRegular :: Int -> Bool -> Int -> Int
peerRegular steps keep n =
    if steps > 2 then topRegular (steps - 1) keep n else topRegular steps keep n

{-# INLINABLE topBreaker #-}
topBreaker :: Int -> Bool -> Int -> Int
topBreaker steps keep n =
    if steps <= 0
      then consume ((n, \v -> if keep then v + 1 else 0) :: Hidden)
      else peerBreaker (steps - 1) keep n

{-# INLINE peerBreaker #-}
peerBreaker :: Int -> Bool -> Int -> Int
peerBreaker steps keep n =
    if steps > 2 then topBreaker (steps - 1) keep n else topBreaker steps keep n

{-# INLINE openedRec #-}
openedRec :: Int -> Int -> Int
openedRec steps n =
    if steps <= 0
      then case hide n of (v, render) -> render v
      else nextRec (steps - 1) n

{-# NOINLINE nextRec #-}
nextRec :: Int -> Int -> Int
nextRec steps n =
    if steps > 2 then openedRec (steps - 1) n else openedRec steps n
