{-# LANGUAGE Haskell2010 #-}
module TestCoreInspectPlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \opts todos -> pure (CoreDoPluginPass "InspectDependentCore" (inspect opts) : todos)
  , pluginRecompile  = purePlugin
  }
  where
    inspect opts guts = do
      case opts of
        path : _ -> liftIO $ appendFile path
          ("core:" ++ moduleNameString (moduleName (mg_module guts))
           ++ ":" ++ show (length (flattenBinds (mg_binds guts))) ++ "\n")
        [] -> pure ()
      pure guts
