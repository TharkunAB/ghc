{-# LANGUAGE GHC2024, DependentTypes, ListTuplePuns, TemplateHaskell #-}
module Main where

import Data.Type.Equality
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (RetainedGadtArg(..))
import NativeRetainedConstructorContextPlain (runKnown, runPlain)
import qualified Provider as P

$(do
  let parent = mkName "Copy"
      constructor = mkName "Copy"
      rename (AppT f a) = AppT (rename f) a
      rename (ConT n) | n == ''P.Stored = ConT parent
      rename ty = ty
      copy (ForallMatchC annotation con) = ForallMatchC annotation <$> copy con
      copy (ForallC bs ctx (RetainedGadtC [_] args result))
        | [_, _] <- ctx
        , [GadtIndexArg flag _, GadtValueArg _ _ (AppT _ (IndexVarT use))] <- args
        , flag == use =
            pure (ForallC bs ctx (RetainedGadtC [constructor] args (rename result)))
      copy con = fail (show con)
  original <- reify ''P.Stored
  case original of
    TyConI (DataD ctx _ bs result [con] ds) -> do
      con' <- copy con
      pure [DataD ctx parent bs result [con'] ds]
    _ -> fail (show original))

$(do
  declarations <- [d|
    data Quoted a b where
      Quoted :: forall a b. (a ~ Bool, b ~ Char) =>
                foreach (flag :: Bool) -> P.Payload flag -> Quoted a b
    |]
  let isRetainedContext (ForallMatchC _ con) = isRetainedContext con
      isRetainedContext (ForallC _ [_, _] (RetainedGadtC [_] _ _)) = True
      isRetainedContext _ = False
  case declarations of
    [DataD _ _ _ _ [con] _] | isRetainedContext con -> pure declarations
    _ -> fail (show declarations))

readCopy :: Copy a b -> (a :~: Bool, b :~: Char, String)
readCopy (Copy True n) = (Refl, Refl, show n)
readCopy (Copy False c) = (Refl, Refl, [c])

readQuoted :: Quoted a b -> (a :~: Bool, b :~: Char, String)
readQuoted (Quoted True n) = (Refl, Refl, show n)
readQuoted (Quoted False c) = (Refl, Refl, [c])

main :: IO ()
main = do
  runKnown
  runPlain
  case (readCopy (Copy True 71 :: Copy Bool Char),
        readCopy (Copy False 'r' :: Copy Bool Char)) of
    ((Refl, Refl, x), (Refl, Refl, y)) -> print (x, y)
  case (readQuoted (Quoted True 71 :: Quoted Bool Char),
        readQuoted (Quoted False 'r' :: Quoted Bool Char)) of
    ((Refl, Refl, x), (Refl, Refl, y)) -> print (x, y)
