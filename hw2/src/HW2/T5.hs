module HW2.T5 where

import Control.Monad
import HW2.EXPR
import HW2.T1
import HW2.T2
import HW2.T3

data ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES $ (mapExcept (mapAnnotated f)) . (runES state)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState (ES f) = ES $ \s -> case (f s) of
  Error e -> Error e
  Success (a :# s1) -> runES a s1

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# (f s))

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState
  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

data EvaluationError = DivideByZero deriving Show

binary :: (Double -> Double -> Option EvaluationError) -> (Double -> Double -> Double) -> (Double -> Double -> Prim Double) -> Expr -> Expr -> ExceptState EvaluationError [Prim Double] Double
binary valid oper expr a b = do
  x <- eval a
  y <- eval b
  ES $ \s -> case valid x y of
    Some e -> Error e
    None -> Success ((oper x y) :# (expr x y : s))

unary :: (Double -> Double) -> (Double -> Prim Double) -> Expr -> ExceptState EvaluationError [Prim Double] Double
unary oper expr a = do
  x <- eval a
  ES $ \s -> Success ((oper x) :# (expr x: s))

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Val x) = pure x
eval (Op (Add a b)) = binary (\_ -> \_ -> None) (+) Add a b
eval (Op (Mul a b)) = binary (\_ -> \_ -> None) (*) Mul a b
eval (Op (Sub a b)) = binary (\_ -> \_ -> None) (-) Sub a b
eval (Op (Div a b)) = binary (\_ -> \y -> if 0 == y then Some DivideByZero else None) (/) Div a b
eval (Op (Abs a)) = unary abs Abs a
eval (Op (Sgn a)) = unary signum Sgn a
