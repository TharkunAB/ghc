{-# LANGUAGE Haskell2010 #-}
module TestTcPlugin (plugin) where

import GHC.Plugins
import GHC.Tc.Types (TcPlugin(..), TcPluginSolveResult(..), unsafeTcPluginTcM)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = \opts -> Just $ TcPlugin
      { tcPluginInit    = unsafeTcPluginTcM $ liftIO $ do
          case opts of
            path : _ -> appendFile path "tc-init\n"
            []       -> pure ()
      , tcPluginSolve    = \_ _ _ _ -> pure (TcPluginOk [] [])
      , tcPluginRewrite  = \_ -> emptyUFM
      , tcPluginPostTc   = \_ -> pure ()
      , tcPluginShutdown = \_ -> pure ()
      }
  , pluginRecompile = purePlugin
  }
