module HW2.T4
  (
    Expr (..)
  , Prim (..)
  , State (..)
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import Control.Monad
import HW2.T1

data Prim a
  = Add a a -- (+)
  | Sub a a -- (-)
  | Mul a a -- (*)
  | Div a a -- (/)
  | Abs a -- abs
  | Sgn a -- signum
  deriving (Show)

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x * y = Op (Mul x y)
  x - y = Op (Sub x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)
  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)
  fromRational x = Val (fromRational x)

data State s a = S {runS :: s -> Annotated s a}

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ mapAnnotated f . runS state

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState (S f) = S $ \s -> case f s of
  a :# s1 -> runS a s1

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

binary :: (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> Expr -> Expr -> State [Prim Double] Double
binary oper expr a b = do
  x <- eval a
  y <- eval b
  S $ \s -> oper x y :# (expr x y : s)

unary :: (Double -> Double) -> (Double -> Prim Double) -> Expr -> State [Prim Double] Double
unary oper expr a = do
  x <- eval a
  S $ \s -> oper x :# (expr x: s)

eval :: Expr -> State [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add a b)) = binary (+) Add a b
eval (Op (Mul a b)) = binary (*) Mul a b
eval (Op (Sub a b)) = binary (-) Sub a b
eval (Op (Div a b)) = binary (/) Div a b
eval (Op (Abs a)) = unary abs Abs a
eval (Op (Sgn a)) = unary signum Sgn a
