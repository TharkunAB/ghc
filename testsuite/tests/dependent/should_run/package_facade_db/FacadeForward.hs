{-# LANGUAGE CPP, DataKinds #-}
#ifdef WRONG
{-# LANGUAGE NoDependentTypes #-}
#else
{-# LANGUAGE DependentTypes, ListTuplePuns #-}
#endif
module FacadeForward where

import PublicVector (CharVector, Nat(..), Vec, storedLength, storedCells)

#ifdef WRONG
forward :: CharVector -> Vec Char 'Z
#else
forward :: foreach (p :: CharVector) -> Vec Char (storedLength p)
#endif
forward p = storedCells p
