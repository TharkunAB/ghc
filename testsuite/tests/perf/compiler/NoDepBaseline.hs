{-# LANGUAGE GHC2024 #-}

-- | Non-dependent GHC2024 baseline module.
--
-- This module intentionally leaves DependentTypes disabled so that compiler
-- performance tracking detects any allocation or residency overhead introduced
-- on ordinary Haskell code (records, type classes, GADTs, and exported tuple
-- functions).
module NoDepBaseline
  ( Severity (..)
  , Span (..)
  , MetricRecord (..)
  , Tree (..)
  , Expr (..)
  , Summary (..)
  , Accumulator (..)
  , evalExpr
  , foldTree
  , insertTree
  , balanceMetric
  , mergeSummaries
  , pairWorker01
  , pairWorker02
  , pairWorker03
  , pairWorker04
  , pairWorker05
  , pairWorker06
  , pairWorker07
  , pairWorker08
  , pairWorker09
  , pairWorker10
  , pairWorker11
  , pairWorker12
  , pairWorker13
  , pairWorker14
  , pairWorker15
  , pairWorker16
  , pairWorker17
  , pairWorker18
  , pairWorker19
  , pairWorker20
  , pairWorker21
  , pairWorker22
  , pairWorker23
  , pairWorker24
  , pairWorker25
  , pairWorker26
  , pairWorker27
  , pairWorker28
  , pairWorker29
  , pairWorker30
  , pairWorker31
  , pairWorker32
  , pairWorker33
  , pairWorker34
  , pairWorker35
  , pairWorker36
  , pairWorker37
  , pairWorker38
  , pairWorker39
  , pairWorker40
  , pairWorker41
  , pairWorker42
  , pairWorker43
  , pairWorker44
  , pairWorker45
  , pairWorker46
  , pairWorker47
  , pairWorker48
  , pairWorker49
  , pairWorker50
  , scalarWorker01
  , scalarWorker02
  , scalarWorker03
  , scalarWorker04
  , scalarWorker05
  , scalarWorker06
  , scalarWorker07
  , scalarWorker08
  , scalarWorker09
  , scalarWorker10
  , scalarWorker11
  , scalarWorker12
  , scalarWorker13
  , scalarWorker14
  , scalarWorker15
  , scalarWorker16
  , scalarWorker17
  , scalarWorker18
  , scalarWorker19
  , scalarWorker20
  ) where

import Data.List (foldl')

-- | Diagnostic severity levels.
data Severity = SevDebug | SevInfo | SevWarn | SevError
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Source span coordinates.
data Span = MkSpan
  { spanFile   :: !Int
  , spanStartL :: !Int
  , spanStartC :: !Int
  , spanEndL   :: !Int
  , spanEndC   :: !Int
  }
  deriving (Eq, Ord, Show)

-- | Structured record with strict fields.
data MetricRecord = MkMetricRecord
  { mrId       :: !Int
  , mrSeverity :: !Severity
  , mrSpan     :: !Span
  , mrCount    :: !Int
  , mrWeight   :: !Int
  , mrFlag     :: !Bool
  }
  deriving (Eq, Show)

-- | Polymorphic binary search tree.
data Tree k v
  = Tip
  | Bin !Int !k v !(Tree k v) !(Tree k v)
  deriving (Eq, Show)

-- | Ordinary GHC2024 GADT expression language.
data Expr a where
  ELitInt  :: !Int -> Expr Int
  ELitBool :: !Bool -> Expr Bool
  EAdd     :: Expr Int -> Expr Int -> Expr Int
  EMul     :: Expr Int -> Expr Int -> Expr Int
  ENeg     :: Expr Int -> Expr Int
  EEq      :: Expr Int -> Expr Int -> Expr Bool
  EAnd     :: Expr Bool -> Expr Bool -> Expr Bool
  EIf      :: Expr Bool -> Expr a -> Expr a -> Expr a
  EPair    :: Expr a -> Expr b -> Expr (a, b)
  EFst     :: Expr (a, b) -> Expr a
  ESnd     :: Expr (a, b) -> Expr b

-- | Aggregated summary record.
data Summary = MkSummary
  { sumTotal  :: !Int
  , sumPeak   :: !Int
  , sumErrors :: !Int
  , sumActive :: !Bool
  }
  deriving (Eq, Show)

-- | Type class for summary accumulation.
class Accumulator a where
  toSummary :: a -> Summary
  scaleItem :: Int -> a -> a

instance Accumulator Int where
  toSummary n = MkSummary n (abs n) (if n < 0 then 1 else 0) (n /= 0)
  scaleItem k n = k * n

instance Accumulator MetricRecord where
  toSummary r = MkSummary
    { sumTotal  = mrCount r * mrWeight r
    , sumPeak   = max (mrCount r) (mrWeight r)
    , sumErrors = if mrSeverity r == SevError then 1 else 0
    , sumActive = mrFlag r
    }
  scaleItem k r = r { mrCount = mrCount r * k, mrWeight = mrWeight r + k }

instance (Accumulator a, Accumulator b) => Accumulator (a, b) where
  toSummary (a, b) = mergeSummaries (toSummary a) (toSummary b)
  scaleItem k (a, b) = (scaleItem k a, scaleItem k b)

-- | Combine two Summary values.
mergeSummaries :: Summary -> Summary -> Summary
mergeSummaries a b = MkSummary
  { sumTotal  = sumTotal a + sumTotal b
  , sumPeak   = max (sumPeak a) (sumPeak b)
  , sumErrors = sumErrors a + sumErrors b
  , sumActive = sumActive a || sumActive b
  }

-- | Evaluate an ordinary GADT expression.
evalExpr :: Expr a -> a
evalExpr (ELitInt n) = n
evalExpr (ELitBool b) = b
evalExpr (EAdd x y) = evalExpr x + evalExpr y
evalExpr (EMul x y) = evalExpr x * evalExpr y
evalExpr (ENeg x) = negate (evalExpr x)
evalExpr (EEq x y) = evalExpr x == evalExpr y
evalExpr (EAnd x y) = evalExpr x && evalExpr y
evalExpr (EIf c t e) = if evalExpr c then evalExpr t else evalExpr e
evalExpr (EPair a b) = (evalExpr a, evalExpr b)
evalExpr (EFst p) = fst (evalExpr p)
evalExpr (ESnd p) = snd (evalExpr p)

treeSize :: Tree k v -> Int
treeSize Tip = 0
treeSize (Bin sz _ _ _ _) = sz

-- | Insert or update a key-value entry in a binary search tree.
insertTree :: Ord k => k -> v -> Tree k v -> Tree k v
insertTree k v Tip = Bin 1 k v Tip Tip
insertTree k v (Bin _ kx vx l r) =
  case compare k kx of
    LT -> let l' = insertTree k v l in Bin (1 + treeSize l' + treeSize r) kx vx l' r
    EQ -> Bin (1 + treeSize l + treeSize r) k v l r
    GT -> let r' = insertTree k v r in Bin (1 + treeSize l + treeSize r') kx vx l r'

-- | Fold over all key-value pairs in a Tree.
foldTree :: (k -> v -> b -> b) -> b -> Tree k v -> b
foldTree _ z Tip = z
foldTree f z (Bin _ k v l r) = foldTree f (f k v (foldTree f z r)) l

-- | Compute a balanced metric score and updated MetricRecord.
balanceMetric :: Int -> MetricRecord -> (Int, MetricRecord)
balanceMetric bias r =
  let spanWidth = (spanEndL (mrSpan r) - spanStartL (mrSpan r)) * 80
                + (spanEndC (mrSpan r) - spanStartC (mrSpan r))
      sevBonus = case mrSeverity r of
        SevDebug -> 1
        SevInfo  -> 3
        SevWarn  -> 7
        SevError -> 15
      nextCount = mrCount r + sevBonus + bias
      nextWeight = mrWeight r + max 1 spanWidth
      r' = r { mrCount = nextCount, mrWeight = nextWeight, mrFlag = nextCount > nextWeight }
  in (nextCount * nextWeight, r')

-- | Exported ordinary tuple worker 01.
pairWorker01 :: Int -> MetricRecord -> (Summary, Int)
pairWorker01 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 4) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 4))
                 (EAdd (ELitInt score1) (ELitInt 1))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 02.
pairWorker02 :: Int -> MetricRecord -> (Summary, Int)
pairWorker02 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 7) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 7))
                 (EAdd (ELitInt score1) (ELitInt 2))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 03.
pairWorker03 :: Int -> MetricRecord -> (Summary, Int)
pairWorker03 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 10) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 10))
                 (EAdd (ELitInt score1) (ELitInt 3))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 04.
pairWorker04 :: Int -> MetricRecord -> (Summary, Int)
pairWorker04 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 13) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 13))
                 (EAdd (ELitInt score1) (ELitInt 4))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 05.
pairWorker05 :: Int -> MetricRecord -> (Summary, Int)
pairWorker05 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 16) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 16))
                 (EAdd (ELitInt score1) (ELitInt 5))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 06.
pairWorker06 :: Int -> MetricRecord -> (Summary, Int)
pairWorker06 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 19) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 19))
                 (EAdd (ELitInt score1) (ELitInt 6))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 07.
pairWorker07 :: Int -> MetricRecord -> (Summary, Int)
pairWorker07 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 22) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 22))
                 (EAdd (ELitInt score1) (ELitInt 7))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 08.
pairWorker08 :: Int -> MetricRecord -> (Summary, Int)
pairWorker08 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 25) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 25))
                 (EAdd (ELitInt score1) (ELitInt 8))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 09.
pairWorker09 :: Int -> MetricRecord -> (Summary, Int)
pairWorker09 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 28) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 28))
                 (EAdd (ELitInt score1) (ELitInt 9))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 10.
pairWorker10 :: Int -> MetricRecord -> (Summary, Int)
pairWorker10 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 31) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 31))
                 (EAdd (ELitInt score1) (ELitInt 10))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 11.
pairWorker11 :: Int -> MetricRecord -> (Summary, Int)
pairWorker11 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 34) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 34))
                 (EAdd (ELitInt score1) (ELitInt 11))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 12.
pairWorker12 :: Int -> MetricRecord -> (Summary, Int)
pairWorker12 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 37) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 37))
                 (EAdd (ELitInt score1) (ELitInt 12))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 13.
pairWorker13 :: Int -> MetricRecord -> (Summary, Int)
pairWorker13 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 40) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 40))
                 (EAdd (ELitInt score1) (ELitInt 13))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 14.
pairWorker14 :: Int -> MetricRecord -> (Summary, Int)
pairWorker14 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 43) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 43))
                 (EAdd (ELitInt score1) (ELitInt 14))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 15.
pairWorker15 :: Int -> MetricRecord -> (Summary, Int)
pairWorker15 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 46) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 46))
                 (EAdd (ELitInt score1) (ELitInt 15))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 16.
pairWorker16 :: Int -> MetricRecord -> (Summary, Int)
pairWorker16 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 49) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 49))
                 (EAdd (ELitInt score1) (ELitInt 16))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 17.
pairWorker17 :: Int -> MetricRecord -> (Summary, Int)
pairWorker17 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 52) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 52))
                 (EAdd (ELitInt score1) (ELitInt 17))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 18.
pairWorker18 :: Int -> MetricRecord -> (Summary, Int)
pairWorker18 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 55) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 55))
                 (EAdd (ELitInt score1) (ELitInt 18))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 19.
pairWorker19 :: Int -> MetricRecord -> (Summary, Int)
pairWorker19 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 58) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 58))
                 (EAdd (ELitInt score1) (ELitInt 19))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 20.
pairWorker20 :: Int -> MetricRecord -> (Summary, Int)
pairWorker20 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 61) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 61))
                 (EAdd (ELitInt score1) (ELitInt 20))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 21.
pairWorker21 :: Int -> MetricRecord -> (Summary, Int)
pairWorker21 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 64) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 64))
                 (EAdd (ELitInt score1) (ELitInt 21))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 22.
pairWorker22 :: Int -> MetricRecord -> (Summary, Int)
pairWorker22 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 67) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 67))
                 (EAdd (ELitInt score1) (ELitInt 22))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 23.
pairWorker23 :: Int -> MetricRecord -> (Summary, Int)
pairWorker23 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 70) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 70))
                 (EAdd (ELitInt score1) (ELitInt 23))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 24.
pairWorker24 :: Int -> MetricRecord -> (Summary, Int)
pairWorker24 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 73) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 73))
                 (EAdd (ELitInt score1) (ELitInt 24))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 25.
pairWorker25 :: Int -> MetricRecord -> (Summary, Int)
pairWorker25 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 76) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 76))
                 (EAdd (ELitInt score1) (ELitInt 25))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 26.
pairWorker26 :: Int -> MetricRecord -> (Summary, Int)
pairWorker26 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 79) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 79))
                 (EAdd (ELitInt score1) (ELitInt 26))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 27.
pairWorker27 :: Int -> MetricRecord -> (Summary, Int)
pairWorker27 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 82) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 82))
                 (EAdd (ELitInt score1) (ELitInt 27))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 28.
pairWorker28 :: Int -> MetricRecord -> (Summary, Int)
pairWorker28 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 85) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 85))
                 (EAdd (ELitInt score1) (ELitInt 28))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 29.
pairWorker29 :: Int -> MetricRecord -> (Summary, Int)
pairWorker29 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 88) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 88))
                 (EAdd (ELitInt score1) (ELitInt 29))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 30.
pairWorker30 :: Int -> MetricRecord -> (Summary, Int)
pairWorker30 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 91) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 91))
                 (EAdd (ELitInt score1) (ELitInt 30))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 31.
pairWorker31 :: Int -> MetricRecord -> (Summary, Int)
pairWorker31 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 94) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 94))
                 (EAdd (ELitInt score1) (ELitInt 31))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 32.
pairWorker32 :: Int -> MetricRecord -> (Summary, Int)
pairWorker32 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 97) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 97))
                 (EAdd (ELitInt score1) (ELitInt 32))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 33.
pairWorker33 :: Int -> MetricRecord -> (Summary, Int)
pairWorker33 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 100) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 100))
                 (EAdd (ELitInt score1) (ELitInt 33))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 34.
pairWorker34 :: Int -> MetricRecord -> (Summary, Int)
pairWorker34 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 103) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 103))
                 (EAdd (ELitInt score1) (ELitInt 34))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 35.
pairWorker35 :: Int -> MetricRecord -> (Summary, Int)
pairWorker35 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 106) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 106))
                 (EAdd (ELitInt score1) (ELitInt 35))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 36.
pairWorker36 :: Int -> MetricRecord -> (Summary, Int)
pairWorker36 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 109) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 109))
                 (EAdd (ELitInt score1) (ELitInt 36))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 37.
pairWorker37 :: Int -> MetricRecord -> (Summary, Int)
pairWorker37 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 112) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 112))
                 (EAdd (ELitInt score1) (ELitInt 37))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 38.
pairWorker38 :: Int -> MetricRecord -> (Summary, Int)
pairWorker38 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 115) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 115))
                 (EAdd (ELitInt score1) (ELitInt 38))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 39.
pairWorker39 :: Int -> MetricRecord -> (Summary, Int)
pairWorker39 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 118) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 118))
                 (EAdd (ELitInt score1) (ELitInt 39))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 40.
pairWorker40 :: Int -> MetricRecord -> (Summary, Int)
pairWorker40 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 121) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 121))
                 (EAdd (ELitInt score1) (ELitInt 40))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 41.
pairWorker41 :: Int -> MetricRecord -> (Summary, Int)
pairWorker41 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 124) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 124))
                 (EAdd (ELitInt score1) (ELitInt 41))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 42.
pairWorker42 :: Int -> MetricRecord -> (Summary, Int)
pairWorker42 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 127) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 127))
                 (EAdd (ELitInt score1) (ELitInt 42))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 43.
pairWorker43 :: Int -> MetricRecord -> (Summary, Int)
pairWorker43 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 130) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 130))
                 (EAdd (ELitInt score1) (ELitInt 43))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 44.
pairWorker44 :: Int -> MetricRecord -> (Summary, Int)
pairWorker44 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 133) rec0
      rec2 = scaleItem 4 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 133))
                 (EAdd (ELitInt score1) (ELitInt 44))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 4))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 45.
pairWorker45 :: Int -> MetricRecord -> (Summary, Int)
pairWorker45 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 136) rec0
      rec2 = scaleItem 5 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 136))
                 (EAdd (ELitInt score1) (ELitInt 45))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 5))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 46.
pairWorker46 :: Int -> MetricRecord -> (Summary, Int)
pairWorker46 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 139) rec0
      rec2 = scaleItem 6 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 139))
                 (EAdd (ELitInt score1) (ELitInt 46))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 6))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 47.
pairWorker47 :: Int -> MetricRecord -> (Summary, Int)
pairWorker47 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 142) rec0
      rec2 = scaleItem 7 (rec1 { mrSeverity = SevError })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 142))
                 (EAdd (ELitInt score1) (ELitInt 47))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 7))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 48.
pairWorker48 :: Int -> MetricRecord -> (Summary, Int)
pairWorker48 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 145) rec0
      rec2 = scaleItem 8 (rec1 { mrSeverity = SevDebug })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 145))
                 (EAdd (ELitInt score1) (ELitInt 48))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 8))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 49.
pairWorker49 :: Int -> MetricRecord -> (Summary, Int)
pairWorker49 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 148) rec0
      rec2 = scaleItem 2 (rec1 { mrSeverity = SevInfo })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 148))
                 (EAdd (ELitInt score1) (ELitInt 49))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 2))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary tuple worker 50.
pairWorker50 :: Int -> MetricRecord -> (Summary, Int)
pairWorker50 seed rec0 =
  let (score1, rec1) = balanceMetric (seed + 151) rec0
      rec2 = scaleItem 3 (rec1 { mrSeverity = SevWarn })
      expr = EIf (EEq (ELitInt (mrCount rec2)) (ELitInt 151))
                 (EAdd (ELitInt score1) (ELitInt 50))
                 (EMul (ELitInt (mrWeight rec2)) (ELitInt 3))
      val = evalExpr expr
      s = mergeSummaries (toSummary rec1) (toSummary (rec2, val))
  in (s, val + sumTotal s)

-- | Exported ordinary scalar worker 01.
scalarWorker01 :: [Int] -> MetricRecord -> Int
scalarWorker01 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker01 (x + 1) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 02.
scalarWorker02 :: [Int] -> MetricRecord -> Int
scalarWorker02 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker02 (x + 2) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 03.
scalarWorker03 :: [Int] -> MetricRecord -> Int
scalarWorker03 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker03 (x + 3) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 04.
scalarWorker04 :: [Int] -> MetricRecord -> Int
scalarWorker04 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker04 (x + 4) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 05.
scalarWorker05 :: [Int] -> MetricRecord -> Int
scalarWorker05 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker05 (x + 5) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 06.
scalarWorker06 :: [Int] -> MetricRecord -> Int
scalarWorker06 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker06 (x + 6) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 07.
scalarWorker07 :: [Int] -> MetricRecord -> Int
scalarWorker07 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker07 (x + 7) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 08.
scalarWorker08 :: [Int] -> MetricRecord -> Int
scalarWorker08 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker08 (x + 8) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 09.
scalarWorker09 :: [Int] -> MetricRecord -> Int
scalarWorker09 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker09 (x + 9) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 10.
scalarWorker10 :: [Int] -> MetricRecord -> Int
scalarWorker10 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker10 (x + 10) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 11.
scalarWorker11 :: [Int] -> MetricRecord -> Int
scalarWorker11 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker11 (x + 11) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 12.
scalarWorker12 :: [Int] -> MetricRecord -> Int
scalarWorker12 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker12 (x + 12) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 13.
scalarWorker13 :: [Int] -> MetricRecord -> Int
scalarWorker13 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker13 (x + 13) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 14.
scalarWorker14 :: [Int] -> MetricRecord -> Int
scalarWorker14 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker14 (x + 14) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 15.
scalarWorker15 :: [Int] -> MetricRecord -> Int
scalarWorker15 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker15 (x + 15) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 16.
scalarWorker16 :: [Int] -> MetricRecord -> Int
scalarWorker16 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker16 (x + 16) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 17.
scalarWorker17 :: [Int] -> MetricRecord -> Int
scalarWorker17 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker17 (x + 17) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 18.
scalarWorker18 :: [Int] -> MetricRecord -> Int
scalarWorker18 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker18 (x + 18) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 19.
scalarWorker19 :: [Int] -> MetricRecord -> Int
scalarWorker19 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker19 (x + 19) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree

-- | Exported ordinary scalar worker 20.
scalarWorker20 :: [Int] -> MetricRecord -> Int
scalarWorker20 xs rec0 =
  let step (treeAcc, sumAcc) x =
        let (s1, v1) = pairWorker20 (x + 20) rec0
            merged = mergeSummaries sumAcc s1
        in (insertTree x v1 treeAcc, merged)
      (finalTree, finalSum) = foldl' step (Tip, MkSummary 0 0 0 False) xs
  in foldTree (\k v acc -> acc + k * v) (sumTotal finalSum + sumPeak finalSum) finalTree
