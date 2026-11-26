{-# LANGUAGE ExistentialTypes, ExtendedLet, GADTs, DataKinds #-}
module NativeProposal378AliasChecks (LetPair, ExShow) where
type LetPair a = let b = Maybe a in (b, b)
type ExShow = exists a. a
