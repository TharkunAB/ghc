{-# LANGUAGE DependentTypes #-}
module BadLookup (Key(..), Env(..), lookupEnvBad, law) where

import Data.Type.Equality ((:~:) (Refl))
import Prelude (Bool(..))

data Key = K0 | K1
data Env where
  ENil  :: Env
  ECons :: Bool -> Env -> Env

lookupEnvBad :: Env -> Key -> Bool
lookupEnvBad ENil        _  = False
lookupEnvBad (ECons b _) K0 = b
lookupEnvBad (ECons _ e) K1 = lookupEnvBad e K0

law :: lookupEnvBad ENil K0 :~: False
law = Refl
