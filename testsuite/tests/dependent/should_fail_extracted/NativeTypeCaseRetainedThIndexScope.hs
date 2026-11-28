{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThIndexScope where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (getQ, putQ)

keepIdx :: foreach (flag :: Bool) ->
  $(do Just n <- lookupValueName "flag"; putQ n; [t| Int |])
keepIdx _ = 1

$(pure [])

escapedIdx :: $(do Just n <- getQ; pure (IndexVarT n))
escapedIdx = undefined

erasedIdx :: forall (flag :: Bool). $(do Just n <- lookupTypeName "flag"; pure (IndexVarT n))
erasedIdx = undefined

freshIdx :: foreach (flag :: Bool). $(do n <- newName "flag"; pure (IndexVarT n))
freshIdx = undefined
