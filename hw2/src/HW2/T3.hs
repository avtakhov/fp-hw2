module HW2.T3 where

import HW2.T1
import HW2.T2

joinOption    :: Option (Option a) -> Option a
joinOption None = None
joinOption (Some a) = a

joinExcept    :: Except e (Except e a) -> Except e a
joinExcept (Error e) = Error e
joinExcept (Success a) = a

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# (e2 <> e1)

joinList      :: List (List a) -> List a
joinList Nil = Nil
joinList (a :. as) = kConcat a (joinList as)

joinFun       :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> case f i of 
  F g -> g i