{-# LANGUAGE Haskell2010 #-}
module TestCoreMutatePlugin (plugin) where

import GHC.Plugins

plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = \_ todos -> pure ( todos ++ [CoreDoPluginPass "MutateCore" mutate] )
  , pluginRecompile  = purePlugin
  }
  where
    mutate guts = pure (guts { mg_binds = map mutBind (mg_binds guts) })
    mutBind (NonRec b rhs) = NonRec b (mutRhs b rhs)
    mutBind (Rec pairs)    = Rec [(b, mutRhs b rhs) | (b, rhs) <- pairs]

    mutRhs b rhs
      | occNameString (getOccName b) == "eqNat"
      , (bndrs, _body) <- collectBinders rhs
      , [x, y] <- filter isId bndrs
      = mkLams bndrs (Var x `seq` Var y `seq` Var falseDataConId)
      | occNameString (getOccName b) == "lookupEnvBad"
      , (bndrs, _body) <- collectBinders rhs
      , v_last : _ <- reverse (filter isId bndrs)
      , (_, _) <- (v_last, rhs)
      = let swapAlt (Alt con bs e) = Alt con bs (swapBoolExpr e)
            walk (Lam v e) = Lam v (walk e)
            walk (Case s cb ty alts) = Case s cb ty (map swapAlt alts)
            walk e = swapBoolExpr e
        in walk rhs
      | otherwise = rhs

    swapBoolExpr (Var v)
      | v == trueDataConId  = Var falseDataConId
      | v == falseDataConId = Var trueDataConId
    swapBoolExpr (App f a) = App (swapBoolExpr f) (swapBoolExpr a)
    swapBoolExpr (Cast e c) = Cast (swapBoolExpr e) c
    swapBoolExpr e = e
