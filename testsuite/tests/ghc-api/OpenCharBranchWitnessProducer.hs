{-# LANGUAGE LambdaCase, NoDependentTypes #-}
module OpenCharBranchWitnessProducer (classify, otherClassifier) where

classify :: Char -> Char
classify = \case
  'x' -> 'X'
  'é' -> 'É'
  c -> c

otherClassifier :: Char -> Char
otherClassifier = classify
