{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThWrongVar where
import Data.Proxy (Proxy)
import Language.Haskell.TH
import NativeRetainedTH (Select)

wrongVar :: $(do b <- newName "b"
                 pure (ForEachT BndrReq b (ConT ''Bool) (AppT (ConT ''Select) (VarT b))))
wrongVar = undefined

wrongIdxAst :: foreach (flag :: Bool) ->
  $(do Just n <- lookupValueName "flag"; pure (AppT (ConT ''Proxy) (VarT n)))
wrongIdxAst = undefined
