{-# LANGUAGE DependentTypes #-}

-- | Spectral runtime benchmark comparing an intrinsically-typed De Bruijn STLC
-- interpreter against a paired dynamically-tagged plain-Haskell interpreter.
module Main (main) where

import Data.Kind (Type)
import GHC.Tuple (Unit)
import System.Environment (getArgs)

interpMod :: Int
interpMod = 1000000007

-- Intrinsically-typed object language

data Ty
  = TInt
  | TBool
  | TPair Ty Ty
  | TArr Ty Ty

data Ctx
  = Empty
  | Snoc Ctx Ty

type Var :: Ctx -> Ty -> Type
data Var g t where
  VZ :: Var (Snoc g t) t
  VS :: Var g t -> Var (Snoc g s) t

type Expr :: Ctx -> Ty -> Type
data Expr g t where
  EVar  :: Var g t -> Expr g t
  ELit  :: !Int -> Expr g TInt
  EBool :: !Bool -> Expr g TBool
  EAdd  :: Expr g TInt -> Expr g TInt -> Expr g TInt
  EMul  :: Expr g TInt -> Expr g TInt -> Expr g TInt
  EEq   :: Expr g TInt -> Expr g TInt -> Expr g TBool
  EIf   :: Expr g TBool -> Expr g t -> Expr g t -> Expr g t
  EPair :: Expr g a -> Expr g b -> Expr g (TPair a b)
  EFst  :: Expr g (TPair a b) -> Expr g a
  ESnd  :: Expr g (TPair a b) -> Expr g b
  ELam  :: Expr (Snoc g a) b -> Expr g (TArr a b)
  EApp  :: Expr g (TArr a b) -> Expr g a -> Expr g b
  EIter :: !Int -> Expr g a -> Expr g (TArr a a) -> Expr g a

type Val :: Ty -> Type
data Val t where
  VInt  :: !Int -> Val TInt
  VBool :: !Bool -> Val TBool
  VPair :: !(Val a) -> !(Val b) -> Val (TPair a b)
  VFun  :: (Val a -> Val b) -> Val (TArr a b)

type Env :: Ctx -> Type
data Env g where
  ENil  :: Env Empty
  ECons :: !(Val t) -> !(Env g) -> Env (Snoc g t)

lookupEnv :: Var g t -> Env g -> Val t
lookupEnv VZ (ECons v _) = v
lookupEnv (VS x) (ECons _ env) = lookupEnv x env

iterVal :: Int -> Val a -> (Val a -> Val a) -> Val a
iterVal 0 !acc _ = acc
iterVal k !acc f = iterVal (k - 1) (f acc) f

evalDep :: Env g -> Expr g t -> Val t
evalDep env (EVar v) = lookupEnv v env
evalDep _ (ELit n) = VInt n
evalDep _ (EBool b) = VBool b
evalDep env (EAdd a b) =
  case (evalDep env a, evalDep env b) of
    (VInt x, VInt y) -> VInt ((x + y) `rem` interpMod)
evalDep env (EMul a b) =
  case (evalDep env a, evalDep env b) of
    (VInt x, VInt y) -> VInt ((x * y) `rem` interpMod)
evalDep env (EEq a b) =
  case (evalDep env a, evalDep env b) of
    (VInt x, VInt y) -> VBool (x == y)
evalDep env (EIf c t e) =
  case evalDep env c of
    VBool True -> evalDep env t
    VBool False -> evalDep env e
evalDep env (EPair a b) = VPair (evalDep env a) (evalDep env b)
evalDep env (EFst p) =
  case evalDep env p of
    VPair a _ -> a
evalDep env (ESnd p) =
  case evalDep env p of
    VPair _ b -> b
evalDep env (ELam body) = VFun (\arg -> evalDep (ECons arg env) body)
evalDep env (EApp f x) =
  case evalDep env f of
    VFun k -> k (evalDep env x)
evalDep env (EIter count z stepFn) =
  case evalDep env stepFn of
    VFun f -> iterVal count (evalDep env z) f

-- | Program iterating (acc, i) -> (acc + i * 3 + 1, i + 1) for 20 steps.
depProgram :: Int -> Expr Empty TInt
depProgram seed =
  let stepPair :: Expr Empty (TArr (TPair TInt TInt) (TPair TInt TInt))
      stepPair =
        ELam
          (EPair
            (EAdd (EFst (EVar VZ)) (EAdd (EMul (ESnd (EVar VZ)) (ELit 3)) (ELit 1)))
            (EAdd (ESnd (EVar VZ)) (ELit 1)))
      initPair :: Expr Empty (TPair TInt TInt)
      initPair = EPair (ELit (seed `rem` 97)) (ELit 1)
      loopRes = EIter 20 initPair stepPair
  in EIf (EEq (ESnd loopRes) (ELit 0)) (ELit 0) (EAdd (EFst loopRes) (ESnd loopRes))

{-# NOINLINE depInterpStep #-}
depInterpStep :: Int -> Int
depInterpStep seed =
  case evalDep ENil (depProgram seed) of
    VInt r -> r

{-# NOINLINE depInterpBench #-}
depInterpBench :: Int -> Int
depInterpBench iters = go 0 iters
  where
    go !acc 0 = acc
    go !acc i = go ((acc + depInterpStep i) `rem` interpMod) (i - 1)

-- Paired plain-Haskell untyped interpreter

data UExpr
  = UVar !Int
  | ULit !Int
  | UBool !Bool
  | UAdd UExpr UExpr
  | UMul UExpr UExpr
  | UEq UExpr UExpr
  | UIf UExpr UExpr UExpr
  | UPair UExpr UExpr
  | UFst UExpr
  | USnd UExpr
  | ULam UExpr
  | UApp UExpr UExpr
  | UIter !Int UExpr UExpr

data UVal
  = UVInt !Int
  | UVBool !Bool
  | UVPair !UVal !UVal
  | UVFun (UVal -> UVal)

data UEnv
  = UENil
  | UECons !UVal !UEnv

lookupUEnv :: Int -> UEnv -> UVal
lookupUEnv 0 (UECons v _) = v
lookupUEnv k (UECons _ env) = lookupUEnv (k - 1) env
lookupUEnv _ UENil = error "lookupUEnv: unbound variable"

iterUVal :: Int -> UVal -> (UVal -> UVal) -> UVal
iterUVal 0 !acc _ = acc
iterUVal k !acc f = iterUVal (k - 1) (f acc) f

evalPlain :: UEnv -> UExpr -> UVal
evalPlain env (UVar i) = lookupUEnv i env
evalPlain _ (ULit n) = UVInt n
evalPlain _ (UBool b) = UVBool b
evalPlain env (UAdd a b) =
  case (evalPlain env a, evalPlain env b) of
    (UVInt x, UVInt y) -> UVInt ((x + y) `rem` interpMod)
    _ -> error "evalPlain: type error in UAdd"
evalPlain env (UMul a b) =
  case (evalPlain env a, evalPlain env b) of
    (UVInt x, UVInt y) -> UVInt ((x * y) `rem` interpMod)
    _ -> error "evalPlain: type error in UMul"
evalPlain env (UEq a b) =
  case (evalPlain env a, evalPlain env b) of
    (UVInt x, UVInt y) -> UVBool (x == y)
    _ -> error "evalPlain: type error in UEq"
evalPlain env (UIf c t e) =
  case evalPlain env c of
    UVBool True -> evalPlain env t
    UVBool False -> evalPlain env e
    _ -> error "evalPlain: type error in UIf"
evalPlain env (UPair a b) = UVPair (evalPlain env a) (evalPlain env b)
evalPlain env (UFst p) =
  case evalPlain env p of
    UVPair a _ -> a
    _ -> error "evalPlain: type error in UFst"
evalPlain env (USnd p) =
  case evalPlain env p of
    UVPair _ b -> b
    _ -> error "evalPlain: type error in USnd"
evalPlain env (ULam body) = UVFun (\arg -> evalPlain (UECons arg env) body)
evalPlain env (UApp f x) =
  case evalPlain env f of
    UVFun k -> k (evalPlain env x)
    _ -> error "evalPlain: type error in UApp"
evalPlain env (UIter count z stepFn) =
  case evalPlain env stepFn of
    UVFun f -> iterUVal count (evalPlain env z) f
    _ -> error "evalPlain: type error in UIter"

plainProgram :: Int -> UExpr
plainProgram seed =
  let stepPair =
        ULam
          (UPair
            (UAdd (UFst (UVar 0)) (UAdd (UMul (USnd (UVar 0)) (ULit 3)) (ULit 1)))
            (UAdd (USnd (UVar 0)) (ULit 1)))
      initPair = UPair (ULit (seed `rem` 97)) (ULit 1)
      loopRes = UIter 20 initPair stepPair
  in UIf (UEq (USnd loopRes) (ULit 0)) (ULit 0) (UAdd (UFst loopRes) (USnd loopRes))

{-# NOINLINE plainInterpStep #-}
plainInterpStep :: Int -> Int
plainInterpStep seed =
  case evalPlain UENil (plainProgram seed) of
    UVInt r -> r
    _ -> error "plainInterpStep: expected int"

{-# NOINLINE plainInterpBench #-}
plainInterpBench :: Int -> Int
plainInterpBench iters = go 0 iters
  where
    go !acc 0 = acc
    go !acc i = go ((acc + plainInterpStep i) `rem` interpMod) (i - 1)

main :: IO Unit
main = do
  args <- getArgs
  let n = 8000
  case args of
    ["plain"] -> print (plainInterpBench n)
    ["verify"] -> do
      let d = depInterpBench n
          p = plainInterpBench n
      if d == p
        then print d
        else error ("Mismatch: dep=" ++ show d ++ " plain=" ++ show p)
    _ -> print (depInterpBench n)
