{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module Forward (flag, alias, convert) where
import CharacterCase (flag)
import qualified CharacterCase
alias :: Char -> Bool
alias = flag
convert :: Char -> Char
convert = CharacterCase.upper
