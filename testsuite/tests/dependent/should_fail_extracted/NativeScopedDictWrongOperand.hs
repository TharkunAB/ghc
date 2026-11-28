{-# LANGUAGE DataKinds, DependentTypes, GADTs, ListTuplePuns, ScopedTypeVariables #-}
module NativeScopedDictWrongOperand where
import NativeScopedDictExportProvider
forwardBoth :: forall a b. (Choose a, Choose b)
            => foreach (x :: a) -> foreach (y :: b)
            -> (Tagged (choose x), Tagged (choose y))
forwardBoth x y = chooseTaggedBoth y x
