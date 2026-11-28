{-# LANGUAGE Haskell2010 #-}
module TestHoleFitPlugin (plugin) where

import Data.Maybe (isJust)
import GHC.Plugins
import GHC.Tc.Errors.Hole.FitTypes (TypedHole(..))
import GHC.Tc.Types.BasicTypes (TcTyThing(..))
import GHC.Tc.Types.Constraint (Hole(..))
import GHC.Tc.Types.LclEnv (tcl_env, tcl_lcl_ctxt)
import GHC.Tc.Utils.Monad (getLclEnv, newTcRef, readTcRef, writeTcRef)
import GHC.Types.Name.Env (nonDetNameEnvElts)

plugin :: Plugin
plugin = defaultPlugin
  { holeFitPlugin = \_ -> Just $ HoleFitPluginR
      { hfPluginInit = newTcRef False
      , hfPluginRun  = \saw_pair_ref -> HoleFitPlugin
          { candPlugin = \th cs -> do
              let is_tyvar_hole = case th_hole th of
                    Just h  -> isJust (getTyVar_maybe (hole_ty h))
                    Nothing -> False
                  has_global_pair = any (\c -> getOccString c == "Pair") cs
              if is_tyvar_hole && has_global_pair
                then writeTcRef saw_pair_ref True
                else pure ()
              pure cs
          , fitPlugin  = \_ fs -> do
              saw_pair <- readTcRef saw_pair_ref
              lcl <- getLclEnv
              let bndr_occs =
                    [ occNameString (nameOccName nm)
                    | ATyVar nm _ <- nonDetNameEnvElts (tcl_env (tcl_lcl_ctxt lcl)) ]
                  saw_r1 = "r1" `elem` bndr_occs
                  extra_pair = [RawHoleFit (text "sawGlobalPairAtSkolem") | saw_pair]
                  extra_r1   = [RawHoleFit (text "sawSigSkolR1InScope")   | saw_r1]
              pure (RawHoleFit (text "pluginSynthesizedFit") : extra_pair ++ extra_r1 ++ fs)
          }
      , hfPluginStop = \_ -> pure ()
      }
  , pluginRecompile = purePlugin
  }
