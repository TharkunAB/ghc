{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedConThBadBang where
import GHC.Types (Multiplicity(One))
import Language.Haskell.TH

$(do
  b <- newName "b"
  let parent = mkName "BadBang"
      payload = GadtValueArg (Bang NoSourceUnpackedness SourceStrict) (PromotedT 'One) (ConT ''Int)
      binder = GadtIndexArg b (ConT ''Bool)
  pure [DataD [] parent [] Nothing [RetainedGadtC [parent] [binder, payload] (ConT parent)] []])
