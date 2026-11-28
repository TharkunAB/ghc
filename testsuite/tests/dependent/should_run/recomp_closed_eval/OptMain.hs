module Main where
import Data.Proxy
import Producer
import Rule ()
import Value
import Numeric.Natural
observe :: Bool -> (Natural, Natural)
observe b = case keep b of
    Proxy -> (score [], fst (pair 7))
main :: IO ()
main = print (observe (error "unused index"))
