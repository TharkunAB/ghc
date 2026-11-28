{-# LANGUAGE DependentTypes, NoLiberalTypeSynonyms, NoPolyKinds #-}
module NativeSynonymReifyProvider (Apply) where

type Apply f a = f a
