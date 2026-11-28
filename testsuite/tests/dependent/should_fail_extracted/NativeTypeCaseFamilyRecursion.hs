{-# LANGUAGE DependentTypes, ListTuplePuns, TypeFamilies #-}
module NativeTypeCaseFamilyRecursion where
import FamilyCasePayload (Count(..))

type family Same (n :: Count) :: Bool where
  Same ('S n) = let selected = n in case selected of
    Z -> 'True
    S _ -> Same ('S n)

type family Growing (n :: Count) :: Bool where
  Growing ('S n) = case n of
    Z -> 'True
    S prior -> Growing ('S ('S ('S prior)))

type family ReadFlag (flag :: Bool) :: Bool where
  ReadFlag 'True = 'False
  ReadFlag 'False = 'True

type family UserNested (flag :: Bool) :: Bool where
  UserNested flag = case ReadFlag flag of { True -> 'False; False -> 'True }
