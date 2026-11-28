{-# LANGUAGE DataKinds, DependentTypes, ListTuplePuns, TypeFamilies #-}
{-# OPTIONS_GHC -O2 -fno-enable-rewrite-rules -fomit-interface-pragmas -fno-write-if-simplified-core #-}
module NativeClosedMethodApartnessOrigin
  ( ImageAlias, ImageFamily, ImageNested, ImageOpaqueAlias, ImageOpaqueCallback, ImageMultiple, ImageMixed ) where

import NativeClosedMethodApartnessOperand (Input, opaqueNot)

type ImageAlias = Prelude.fmap Prelude.not ('Just 'True)

type family ImageFamily where
  ImageFamily = Prelude.fmap Prelude.not ('Just 'True)

type ImageNested = Prelude.fmap Prelude.not (Prelude.id ('Just 'True))
type ImageOpaqueAlias = Prelude.fmap Prelude.not Input
type ImageOpaqueCallback = Prelude.fmap opaqueNot ('Just 'True)
type ImageMultiple = '(Prelude.fmap Prelude.not ('Just 'True), Prelude.fmap Prelude.not ('Just 'False))
type ImageMixed = '(Prelude.fmap Prelude.not ('Just 'True), Prelude.fmap Prelude.not Input)
