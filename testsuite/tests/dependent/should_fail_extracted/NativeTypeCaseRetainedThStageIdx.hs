{-# LANGUAGE DependentTypes, ListTuplePuns, TemplateHaskell #-}
module NativeTypeCaseRetainedThStageIdx where

stageIdx :: foreach (flag :: Bool) -> $(if flag then [t| Int |] else [t| Int |])
stageIdx = undefined
