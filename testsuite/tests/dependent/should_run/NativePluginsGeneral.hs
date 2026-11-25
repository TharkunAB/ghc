{-# LANGUAGE Haskell2010 #-}
{-# OPTIONS_GHC -Wall -Werror #-}
module Main (main) where

import GHC.Plugins
import GHC.Tc.Utils.Monad (newTcRef)

sampleHoleFit :: [CommandLineOption] -> Maybe HoleFitPluginR
sampleHoleFit _ = Just $ HoleFitPluginR
  { hfPluginInit = newTcRef ()
  , hfPluginRun  = \_ -> HoleFitPlugin
      { candPlugin = \_ cs -> pure cs
      , fitPlugin  = \_ fs -> pure (RawHoleFit (text "pluginSynthesizedFit") : fs)
      }
  , hfPluginStop = \_ -> pure ()
  }

expect :: String -> Bool -> IO ()
expect label ok
  | ok        = pure ()
  | otherwise = error ("Plugin hook predicate failed: " ++ label)

main :: IO ()
main = do
  expect "defaultPlugin has default Core hook"
    (hasDefaultCorePlugin defaultPlugin)
  expect "defaultPlugin has default Late hook"
    (hasDefaultLatePlugin defaultPlugin)
  expect "defaultPlugin is hole-fit-only"
    (isHoleFitOnlyPlugin defaultPlugin)

  let hfPlug = defaultPlugin { holeFitPlugin = sampleHoleFit, pluginRecompile = purePlugin }
      tcPlug = defaultPlugin { tcPlugin = \_ -> Nothing, pluginRecompile = purePlugin }
      srcPlug = defaultPlugin
        { parsedResultAction    = \_ _ r -> pure r
        , typeCheckResultAction = \_ _ e -> pure e
        , interfaceLoadAction   = \_ i -> pure i
        , pluginRecompile       = purePlugin
        }
      corePlug = defaultPlugin { installCoreToDos = \_ x -> pure x, pluginRecompile = purePlugin }
      latePlug = defaultPlugin { latePlugin = \_ _ x -> pure x, pluginRecompile = purePlugin }

  expect "holeFitPlugin keeps default Core & Late"
    (isHoleFitOnlyPlugin hfPlug && hasDefaultCorePlugin hfPlug && hasDefaultLatePlugin hfPlug)
  expect "tcPlugin keeps default Core & Late"
    (not (isHoleFitOnlyPlugin tcPlug) && hasDefaultCorePlugin tcPlug && hasDefaultLatePlugin tcPlug)
  expect "source/iface plugin keeps default Core & Late"
    (not (isHoleFitOnlyPlugin srcPlug) && hasDefaultCorePlugin srcPlug && hasDefaultLatePlugin srcPlug)
  expect "installCoreToDos detected as Core plugin, keeps default Late"
    (not (hasDefaultCorePlugin corePlug) && hasDefaultLatePlugin corePlug)
  expect "latePlugin detected as Late plugin"
    (hasDefaultCorePlugin latePlug && not (hasDefaultLatePlugin latePlug))
