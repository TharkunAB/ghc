{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, GADTs #-}
module NativeMultiParamMethodOpenEqProvider
  ( Row (..)
  , MyNat (..)
  , LabelledMap (..)
  , IndexedMap (..)
  , HasTag (..)
  , SuperLabelledMap (..)
  , SoloMap (..)
  , SoloIndexedMap (..)
  , TaggedSemigroup (..)
  , CheckedMonoidOp (..)
  , OpenMap (..)
  , swapEither
  ) where

-- | Concrete-prefix container with an opaque Int payload and a Bool flag.
data Row e t a
  = EmptyRow
  | FlaggedRow e t
  | ValueRow e a
  deriving (Eq, Show)

-- | Inductive unary natural numbers for binary class method equations.
data MyNat
  = MZero
  | MSucc MyNat
  deriving (Show)

-- | Sum swap helper used as a certified nominal callback even under -fno-code.
swapEither :: Either a b -> Either b a
swapEither (Left x)  = Right x
swapEither (Right y) = Left y

-- | Multi-parameter mapping class with functional dependency.
class LabelledMap e f | f -> e where
  lmap :: (a -> b) -> f a -> f b
  ltag :: f a -> Int

instance LabelledMap e (Either e) where
  lmap _ (Left e)  = Left e
  lmap g (Right x) = Right (g x)
  ltag _ = 1

instance LabelledMap Int (Row Int Bool) where
  lmap _ EmptyRow         = EmptyRow
  lmap _ (FlaggedRow n b) = FlaggedRow n b
  lmap g (ValueRow n x)   = ValueRow n (g x)
  ltag _ = 2

instance LabelledMap e ((,) e) where
  lmap g (e, x) = (e, g x)
  ltag _ = 3

instance LabelledMap () Maybe where
  lmap _ Nothing  = Nothing
  lmap g (Just x) = Just (g x)
  ltag _ = 4

-- | Multi-parameter indexed mapping class whose receiver applies two class parameters.
class IndexedMap f i where
  imap :: (a -> b) -> f i a -> f i b
  icount :: f i a -> Int

instance IndexedMap Either e where
  imap _ (Left e)  = Left e
  imap g (Right x) = Right (g x)
  icount _ = 10

instance IndexedMap (Row Int) Bool where
  imap _ EmptyRow         = EmptyRow
  imap _ (FlaggedRow n b) = FlaggedRow n b
  imap g (ValueRow n x)   = ValueRow n (g x)
  icount _ = 20

instance IndexedMap (,) e where
  imap g (e, x) = (e, g x)
  icount _ = 30

-- | Superclass constraint carried in the dictionary before method fields.
class HasTag e f where
  tagValue :: e -> f a -> Int

instance HasTag e (Either e) where
  tagValue _ _ = 7

instance HasTag Int (Row Int Bool) where
  tagValue n _ = n

-- | Multi-parameter mapping class with a superclass dictionary slot.
class HasTag e f => SuperLabelledMap e f | f -> e where
  slmap :: (a -> b) -> f a -> f b
  sltag :: f a -> Int

instance SuperLabelledMap e (Either e) where
  slmap _ (Left e)  = Left e
  slmap g (Right x) = Right (g x)
  sltag _ = 70

instance SuperLabelledMap Int (Row Int Bool) where
  slmap _ EmptyRow         = EmptyRow
  slmap _ (FlaggedRow n b) = FlaggedRow n b
  slmap g (ValueRow n x)   = ValueRow n (g x)
  sltag _ = 80

-- | Single-method mapping class represented by a NewTyCon dictionary.
class SoloMap f where
  soloMap :: (a -> b) -> f a -> f b

instance SoloMap (Either e) where
  soloMap _ (Left e)  = Left e
  soloMap g (Right x) = Right (g x)

instance SoloMap (Row Int Bool) where
  soloMap _ EmptyRow         = EmptyRow
  soloMap _ (FlaggedRow n b) = FlaggedRow n b
  soloMap g (ValueRow n x)   = ValueRow n (g x)

-- | Single-method multi-parameter mapping class represented by a NewTyCon dictionary.
class SoloIndexedMap f i where
  soloImap :: (a -> b) -> f i a -> f i b

instance SoloIndexedMap Either e where
  soloImap _ (Left e)  = Left e
  soloImap g (Right x) = Right (g x)

-- | Standard Eq instance on MyNat returning Bool.
instance Eq MyNat where
  MZero   == MZero   = True
  MZero   == MSucc _ = False
  MSucc _ == MZero   = False
  MSucc m == MSucc n = m == n

-- | Standard Ord instance on MyNat (carries superclass Eq MyNat) returning Ordering.
instance Ord MyNat where
  compare MZero     MZero     = EQ
  compare MZero     (MSucc _) = LT
  compare (MSucc _) MZero     = GT
  compare (MSucc m) (MSucc n) = compare m n

-- | Standard Semigroup instance on MyNat.
instance Semigroup MyNat where
  MZero   <> n = n
  MSucc m <> n = MSucc (m <> n)

-- | Single-method multi-parameter binary class (NewTyCon dictionary).
class TaggedSemigroup tag a | a -> tag where
  tappend :: a -> a -> a

instance TaggedSemigroup Bool MyNat where
  tappend MZero     n = n
  tappend (MSucc m) n = MSucc (tappend m n)

-- | Multi-parameter binary class with an Eq superclass (DataTyCon dictionary).
class Eq a => CheckedMonoidOp tag a | a -> tag where
  mcombine :: a -> a -> a
  mident :: a

instance CheckedMonoidOp Bool MyNat where
  mcombine MZero     n = n
  mcombine (MSucc m) n = MSucc (mcombine m n)
  mident = MZero

-- | Multi-parameter class WITHOUT functional dependency and WITH competing instances
-- used by negative tests to verify apartness checking at open receivers.
class OpenMap tag f where
  omap :: (a -> b) -> f a -> f b
  otag :: f a -> Int

instance OpenMap Bool (Either e) where
  omap _ (Left e)  = Left e
  omap g (Right x) = Right (g x)
  otag _ = 1

instance OpenMap Int (Either Bool) where
  omap _ (Left b)  = Left b
  omap g (Right x) = Right (g x)
  otag _ = 2
