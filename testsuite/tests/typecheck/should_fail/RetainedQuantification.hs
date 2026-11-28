{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module RetainedQuantification where

type family Select (b :: Bool) where
    Select True = Int
    Select False = Bool

keep :: foreach (b :: Int) -> Select b -> Select b
keep b x = x
