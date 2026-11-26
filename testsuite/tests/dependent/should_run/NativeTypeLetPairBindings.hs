{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeTypeLetPairBindings (AliasCount, Held, KnownPrefix, Rebuild, ShadowCount,
                     NestedCount, NestedRight, NestedHeight,
                     Axis(..), AxisLength, Volume(..), VolumeRows, VolumeFields,
                     Grid(..), GridTotal, GridRows, GridHeld, OpaqueGrid, OpaqueColumn) where

import GHC.TypeNats (Nat, type (+))

type Dimensions = '(2, 3)
type Alias = Dimensions
type AliasCount = let { (rows, _) = Alias; cols = 3 } in rows + cols

type family OpaqueField (n :: Nat) :: Bool
type KnownPrefix (n :: Nat) = let { (rows, _) = '(2, OpaqueField n) } in rows

type Held (dimensions :: (Nat, Nat)) = let { (_, _) = dimensions } in Bool
type Rebuild (dimensions :: (Nat, Nat)) = let { (rows, cols) = dimensions } in '(rows, cols)

type ShadowCount (rows :: Nat) =
  let dimensions = '(rows, 3)
      (rows, cols) = dimensions
  in rows + cols

type NestedCount (width :: Nat) =
  let ((width, _), height) = '( '(width, 'True), 3)
  in width + height

type NestedRight (height :: Nat) =
  let { (width, (_, height)) = '(2, '( 'True, height)) }
  in width + height

type family OpaquePair (n :: Nat) :: (Nat, Bool)
type NestedHeight (n :: Nat) =
  let ((_, _), height) = '(OpaquePair n, 3)
  in height

data Grid = Grid Nat Nat
data Axis = Axis Nat
data Volume = Volume Nat Bool Nat

type AxisLength (axis :: Axis) = let Axis len = axis in len
type VolumeRows (volume :: Volume) = let Volume rows _ _ = volume in rows
type VolumeFields (volume :: Volume) =
  let Volume rows solid depth = volume
  in '(rows, solid, depth)

type GridTotal (dimensions :: Grid) =
  let Grid rows cols = dimensions
  in rows + cols

type GridRows (dimensions :: Grid) =
  let Grid rows _ = dimensions
  in rows

type family OpaqueGrid :: Grid
type family OpaqueColumn :: Nat
type GridHeld = let Grid _ _ = OpaqueGrid in Bool
