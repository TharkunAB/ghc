{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedConThBadScope where
import GHC.Types (Multiplicity(One))
import Language.Haskell.TH
import qualified NativeTypeCaseRetainedConThProvider as P

$(do
  b <- newName "b"
  let parent = mkName "BadScope"
      bang = Bang NoSourceUnpackedness NoSourceStrictness
      payload = GadtValueArg bang (PromotedT 'One) (AppT (ConT ''P.Select) (IndexVarT b))
      binder = GadtIndexArg b (ConT ''Bool)
  pure [DataD [] parent [] Nothing [RetainedGadtC [parent] [payload, binder] (ConT parent)] []])
