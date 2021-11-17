{-# LANGUAGE TupleSections #-}

module HW2.T2 where

import HW2.T1

distOption      :: (Option a, Option b) -> Option (a, b)
distPair        :: (Pair a, Pair b) -> Pair (a, b)

distQuad        :: (Quad a, Quad b) -> Quad (a, b)
distAnnotated   :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distExcept      :: (Except e a, Except e b) -> Except e (a, b)
distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distStream      :: (Stream a, Stream b) -> Stream (a, b)
distList        :: (List a, List b) -> List (a, b)
distFun         :: (Fun i a, Fun i b) -> Fun i (a, b)
distOption (None, _) = None
distOption (_, None) = None
distOption (Some a, Some b) = Some (a, b)

distPair (P a b, P x y) = P (a, x) (b, y)

distQuad (Q a b c d, Q p e n i) = Q (a, p) (b, e) (c, n) (d, i)

distAnnotated (a :# x, b :# y) = (a, b) :# (x <> y)

distExcept (Error e, _) = Error e
distExcept (_, Error e) = Error e
distExcept (Success a, Success b) = Success (a, b)

distPrioritised (High a, Medium b) = High (a, b)
distPrioritised (High a, High b) = High (a, b)
distPrioritised (High a, Low b) = High (a, b)
distPrioritised (Medium a, High b) = High (a, b)
distPrioritised (Low a, High b) = High (a, b)

distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Low a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b) = Medium (a, b)

distPrioritised (Low a, Low b) = Low (a, b)

distStream (a :> as, b :> bs) = (a, b) :> (distStream (as, bs))

getList :: a -> List b -> List (a, b)
getList _ Nil = Nil
getList a (x :. xs) = (a, x) :. (getList a xs)

kConcat:: List a -> List a -> List a
kConcat a Nil  = a
kConcat Nil a = a
kConcat (a :. as) bs = (a :. kConcat as bs)

distList (Nil, _) = Nil
distList (_, Nil) = Nil
distList (a :. as, bs) = kConcat (getList a bs) (distList (as, bs))

distFun (F f, F g) = F (\i -> (f i, g i))

wrapOption      :: a -> Option a
wrapOption = Some

wrapPair        :: a -> Pair a
wrapPair a = P a a

wrapQuad        :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated   :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept      :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream      :: a -> Stream a
wrapStream a = a :> (wrapStream a)

wrapList        :: a -> List a
wrapList a = a :. Nil
