{-# LANGUAGE DependentTypes, ListTuplePuns, MultiParamTypeClasses, TypeFamilies #-}
module NativeConstrainedFamMissingBinder where
class C a b where
    type F a
