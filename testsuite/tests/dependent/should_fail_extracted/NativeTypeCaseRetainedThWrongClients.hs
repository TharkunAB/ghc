{-# LANGUAGE DataKinds, GADTs, NoDependentTypes #-}
module NativeTypeCaseRetainedThWrongClients where
import Data.Proxy (Proxy)
import NativeTypeCaseRetainedThChooseAcross (chooseMaybe)
import NativeTypeCaseRetainedThRaw
  (chooseInline, chooseNested, outerCapturedQuote, retainedCapturedQuote, SpliceFlag(SpliceOn))
import NativeTypeCaseRetainedThRenderAcross (renderChanged, renderMaybe)

wrongBranch :: Int
wrongBranch = chooseInline False

wrong :: Proxy 'False
wrong = chooseNested () SpliceOn

wrongOuter :: Proxy 'False
wrongOuter = outerCapturedQuote SpliceOn

wrongRetained :: Proxy 'False
wrongRetained = retainedCapturedQuote (Just True)

wrongIndex :: Maybe Bool -> Maybe Bool -> String
wrongIndex m n = renderMaybe m (chooseMaybe n)

wrongRow :: Maybe Bool -> String
wrongRow m = renderChanged m (chooseMaybe m)
