{-# LANGUAGE DependentTypes, ListTuplePuns #-}
module NativeRetainedMatchabilityBad where
import GHC.Exts (M)

data Reading (tag :: Bool) (m :: Maybe a) = Reading Int

badOrdinary :: Int %M -> Int
badOrdinary x = x

badRetained :: foreach (tag :: Bool) -> forall a -> foreach (m :: Maybe a) -> Reading tag m %M -> Reading tag m
badRetained _ (type a) _ x = x

badConstructor :: foreach (tag :: Bool) -> Reading tag ('Nothing :: Maybe Bool) -> Int %M -> Int
badConstructor False _ x = x
badConstructor True _ x = x
