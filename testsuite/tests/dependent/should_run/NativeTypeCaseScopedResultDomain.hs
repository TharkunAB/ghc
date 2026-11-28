{-# LANGUAGE NoDependentTypes #-}
module NativeTypeCaseScopedResultDomain (Choice(..)) where
data Choice = Neither | Both Bool Bool
