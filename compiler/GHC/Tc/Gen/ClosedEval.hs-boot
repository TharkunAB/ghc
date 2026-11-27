module GHC.Tc.Gen.ClosedEval where

import GHC.Prelude
import GHC.Core (CoreExpr)
import GHC.Core.Type (Kind, Type)
import GHC.Tc.Types (TcM)
import GHC.Types.Name (Name)
import GHC.Types.Var (Id)

tcPrepareClosedApplication :: Id -> [Type] -> Kind -> [Bool] -> TcM ([CoreExpr] -> TcM CoreExpr)
tcPrepareClosedFunctionValue :: Id -> Type -> TcM CoreExpr
tcPrepareClosedResult :: Name -> Kind -> TcM (CoreExpr -> TcM Type)
tcPrepareClosedObservation :: Name -> Kind -> Name -> TcM (CoreExpr -> TcM (Maybe Type))
tcCheckClosedCallContext :: Name -> TcM ()
tcPreflightClosedCall :: Name -> TcM ()
tcDecodeClosedCallOperand :: Name -> Type -> TcM (Maybe CoreExpr)
tcHasClosedCallExecutable :: Name -> TcM Bool
tcHasClosedValueShape :: Name -> Kind -> TcM Bool
