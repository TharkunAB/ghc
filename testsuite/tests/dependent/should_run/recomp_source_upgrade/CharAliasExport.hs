{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module AliasExport (Alias, Reexport, LocalFlag, LocalUpper) where
import CharacterCase (flagAlias, upperAlias)
import Forward (alias, flag)
type Alias (c :: Char) = alias c
type Reexport (c :: Char) = flag c
type LocalFlag (c :: Char) = flagAlias c
type LocalUpper (c :: Char) = upperAlias c
