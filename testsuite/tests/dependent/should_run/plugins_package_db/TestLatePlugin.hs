{-# LANGUAGE Haskell2010 #-}
module TestLatePlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { latePlugin      = \_ _ x -> pure x
  , pluginRecompile = purePlugin
  }
