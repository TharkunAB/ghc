{-# LANGUAGE BangPatterns, ExplicitForAll, NoDependentTypes #-}
module Builder where
import Numeric.Natural (Natural)

left :: Natural -> (Natural, Natural)
left n = (n, errorWithoutStackTrace "unused right field")
right :: forall b a. b -> a -> (a, a)
right _ x = (errorWithoutStackTrace "unused left field", x)
defined :: Natural -> Natural -> (Natural, Natural)
defined n _ = (n, n + 1)
bad :: Natural -> Natural
bad _ = errorWithoutStackTrace "ignored argument was forced"
