{-# OPTIONS_GHC -fno-omit-interface-pragmas -Wno-orphans -Wno-inline-rule-shadowing #-}
module Rule () where
import Value
{-# RULES "score/empty" score [] = 29 #-}
