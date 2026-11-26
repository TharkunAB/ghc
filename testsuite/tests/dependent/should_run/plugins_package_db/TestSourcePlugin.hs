{-# LANGUAGE Haskell2010 #-}
module TestSourcePlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { parsedResultAction = \opts ms parsed -> do
      case opts of
        path : _ -> liftIO $ appendFile path ("parsed:" ++ moduleNameString (ms_mod_name ms) ++ "\n")
        []       -> pure ()
      pure parsed
  , typeCheckResultAction = \opts ms env -> do
      case opts of
        path : _ -> liftIO $ appendFile path ("typechecked:" ++ moduleNameString (ms_mod_name ms) ++ "\n")
        []       -> pure ()
      pure env
  , interfaceLoadAction = \_ iface -> pure iface
  , pluginRecompile     = purePlugin
  }
