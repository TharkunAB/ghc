{-# LANGUAGE Haskell2010 #-}
module TestHoleFitPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Utils.Monad (newTcRef)

plugin :: Plugin
plugin = defaultPlugin
  { holeFitPlugin = \_ -> Just $ HoleFitPluginR
      { hfPluginInit = newTcRef ()
      , hfPluginRun  = \_ -> HoleFitPlugin
          { candPlugin = \_ cs -> pure cs
          , fitPlugin  = \_ fs -> pure (RawHoleFit (text "pluginSynthesizedFit") : fs)
          }
      , hfPluginStop = \_ -> pure ()
      }
  , pluginRecompile = purePlugin
  }
