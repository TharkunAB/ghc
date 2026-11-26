{-# LANGUAGE DependentTypes #-}
module NativeHigherOrderLargeElim
  ( WireTy (..)
  , Seg (..)
  , Piece (..)
  , Route (..)
  , EndpointId (..)
  , endpointOf
  , interp
  , routeType
  , routeTypeBare
  , sampleHandler
  , sampleLink
  , sampleBareCheck
  , wrapRouteResult
  , checkWrapMaybe
  , Eff (..)
  , IdM (..)
  , ReaderM (..)
  , StateM (..)
  , stackM
  , outStep
  , stackOut
  , runGenericStack
  , sampleStackComputation
  , checkStackOut
  , FieldTag (..)
  , Schema (..)
  , SchemaName (..)
  , schemaOf
  , payload
  , encode
  , encodeInvis
  , sampleUserEncoded
  , sampleUserEncodedInvis
  ) where

import Data.Kind (Type)
import Data.Type.Equality ((:~:) (Refl))
import GHC.Tuple (Tuple2, Unit)
import qualified GHC.Types as GHCTypes

-- ---------------------------------------------------------------------------
-- 1. Higher-order type-function parameter + nested infix constructor patterns
-- ---------------------------------------------------------------------------

data WireTy = WInt | WText | WBool

data Seg = SUsers | SActive

data Piece = Lit Seg | Cap WireTy

infixr 5 :>
data Route = End WireTy | Piece :> Route

data EndpointId = UserEndpoint

endpointOf :: EndpointId -> Route
endpointOf UserEndpoint = Lit SUsers :> Cap WInt :> Lit SActive :> Cap WBool :> End WText

interp :: WireTy -> Type
interp WInt  = type Int
interp WText = type String
interp WBool = type Bool

routeType :: (WireTy -> Type) -> Route -> Type
routeType ret (End res)       = type (ret res)
routeType ret (Lit _ :> rest) = type (routeType ret rest)
routeType ret (Cap w :> rest) = type (interp w -> routeType ret rest)

routeTypeBare :: (WireTy -> Type) -> Route -> Type
routeTypeBare ret (End res)       = ret res
routeTypeBare ret (Lit _ :> rest) = routeTypeBare ret rest
routeTypeBare ret (Cap w :> rest) = (interp w -> routeTypeBare ret rest)

sampleHandler :: routeType interp (endpointOf UserEndpoint)
sampleHandler uid active = show uid ++ ":" ++ show active

sampleLink :: routeType (\_ -> String) (endpointOf UserEndpoint)
sampleLink uid active = "/users/" ++ show uid ++ "/active/" ++ show active

sampleBareCheck :: routeType interp (endpointOf UserEndpoint) :~: routeTypeBare interp (endpointOf UserEndpoint)
sampleBareCheck = Refl

wrapRouteResult :: (Type -> Type) -> Route -> Type
wrapRouteResult f (End res)       = type (f (interp res))
wrapRouteResult f (Lit _ :> rest) = type (wrapRouteResult f rest)
wrapRouteResult f (Cap w :> rest) = type (interp w -> wrapRouteResult f rest)

checkWrapMaybe :: wrapRouteResult Maybe (Cap WInt :> End WBool) :~: (Int -> Maybe Bool)
checkWrapMaybe = Refl

-- ---------------------------------------------------------------------------
-- 2. Higher-kinded `Type -> Type` result + accumulator-updating recursion
-- ---------------------------------------------------------------------------

data Eff = EReader | EState | ECount

newtype IdM a = IdM { runIdM :: a }

instance Functor IdM where
  fmap f (IdM x) = IdM (f x)

instance Applicative IdM where
  pure = IdM
  IdM f <*> IdM x = IdM (f x)

instance Monad IdM where
  IdM x >>= k = k x

newtype ReaderM r (m :: Type -> Type) a = ReaderM { runReaderM :: r -> m a }

instance Functor m => Functor (ReaderM r m) where
  fmap f (ReaderM g) = ReaderM (fmap f . g)

instance Applicative m => Applicative (ReaderM r m) where
  pure x = ReaderM (\_ -> pure x)
  ReaderM ff <*> ReaderM fx = ReaderM (\r -> ff r <*> fx r)

instance Monad m => Monad (ReaderM r m) where
  ReaderM fx >>= k = ReaderM (\r -> fx r >>= \a -> runReaderM (k a) r)

newtype StateM s (m :: Type -> Type) a = StateM { runStateM :: s -> m (Tuple2 a s) }

instance Functor m => Functor (StateM s m) where
  fmap f (StateM g) = StateM (fmap (\(a, s) -> (f a, s)) . g)

instance Monad m => Applicative (StateM s m) where
  pure x = StateM (\s -> pure (x, s))
  StateM ff <*> StateM fx = StateM $ \s0 -> do
    (f, s1) <- ff s0
    (x, s2) <- fx s1
    pure (f x, s2)

instance Monad m => Monad (StateM s m) where
  StateM fx >>= k = StateM $ \s0 -> do
    (a, s1) <- fx s0
    runStateM (k a) s1

stackM :: GHCTypes.List Eff -> Type -> Type
stackM []             = type IdM
stackM (EReader : es) = type (ReaderM String (stackM es))
stackM (EState  : es) = type (StateM Int (stackM es))
stackM (ECount  : es) = type (StateM Int (stackM es))

runGenericStack :: foreach (es :: GHCTypes.List Eff) -> Monad (stackM es) => stackM es Int -> stackM es Int
runGenericStack _ act = do
  x <- act
  pure (x + 1)

sampleStackComputation :: stackM [EReader, EState] Int
sampleStackComputation = runGenericStack [EReader, EState] (pure 41)

outStep :: Eff -> Type -> Type
outStep EReader a = type a
outStep EState  a = type (Tuple2 a Int)
outStep ECount  a = type (Tuple2 a Int)

stackOut :: GHCTypes.List Eff -> Type -> Type
stackOut []       a = type a
stackOut (e : es) a = type (stackOut es (outStep e a))

checkStackOut :: stackOut [EReader, ECount, EState] Bool :~: Tuple2 (Tuple2 Bool Int) Int
checkStackOut = Refl

-- ---------------------------------------------------------------------------
-- 3. Same-module field enum + large elimination + source-table `foreach` call
-- ---------------------------------------------------------------------------

data FieldTag = FId | FActive | FScore
  deriving (Eq, Show)

data Schema = SEnd | SInt FieldTag Schema | SBool FieldTag Schema
  deriving (Eq, Show)

data SchemaName = UserSchema | PostSchema
  deriving (Eq, Show)

schemaOf :: SchemaName -> Schema
schemaOf UserSchema = SInt FId (SBool FActive (SInt FScore SEnd))
schemaOf PostSchema = SInt FId (SBool FActive SEnd)

payload :: Schema -> Type
payload SEnd        = type Unit
payload (SInt _ r)  = type (Tuple2 Int (payload r))
payload (SBool _ r) = type (Tuple2 Bool (payload r))

encode :: foreach (s :: Schema) -> payload s -> GHCTypes.List String
encode SEnd        ()       = []
encode (SInt _ r)  (n, xs)  = show n : encode r xs
encode (SBool _ r) (b, xs)  = show b : encode r xs

encodeInvis :: foreach (s :: Schema). payload s -> GHCTypes.List String
encodeInvis @s xs = encode s xs

sampleUserEncoded :: GHCTypes.List String
sampleUserEncoded = encode (schemaOf UserSchema) (10, (True, (99, ())))

sampleUserEncodedInvis :: GHCTypes.List String
sampleUserEncodedInvis = encodeInvis @(schemaOf UserSchema) (10, (True, (99, ())))
