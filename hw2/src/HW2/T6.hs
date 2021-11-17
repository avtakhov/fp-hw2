{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6 where

import Control.Applicative (Alternative (empty, (<|>)), many, some)
import Control.Monad (MonadPlus, guard, mfilter, replicateM)
import Data.Char (digitToInt, isDigit, isSpace)
import HW2.T1 (Annotated ((:#)), Except (Error, Success))
import Numeric.Natural
import HW2.T4 (Expr(..))
import HW2.T5 (ExceptState(..))

data ParseError = ErrorAtPos Natural deriving (Show)

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving (Functor, Applicative, Monad)

pChar :: Parser Char
pChar = P $
  ES $ \(pos, s) ->
    case s of
      [] -> Error (ErrorAtPos pos)
      (c : cs) -> Success (c :# (pos + 1, cs))

runP :: Parser a -> String -> Except ParseError a
runP (P es) s = case runES es (0, s) of
  Error e -> Error e
  Success (x :# _) -> Success x

parseError :: Parser a
parseError = P (ES $ \(pos, _) -> Error $ ErrorAtPos pos)

instance Alternative Parser where
  empty = parseError
  (<|>) (P (ES p)) (P (ES q)) =
    P
      ( ES $ \s -> case p s of
          Error e -> case q s of
            Error _ -> Error e
            x -> x
          x -> x
      )

pNot :: Parser a -> Parser ()
pNot (P (ES f)) = P $
  ES $ \(pos, s) -> case f (pos, s) of
    Error _ -> Success (() :# (pos, s))
    Success _ -> Error (ErrorAtPos pos)

pEof :: Parser ()
pEof = pNot pChar

skipChars :: Int -> Parser String
skipChars n = replicateM n pChar

instance MonadPlus Parser

pWs :: Parser ()
pWs = do
  _ <- many (mfilter Data.Char.isSpace pChar)
  pure ()

pExpect :: Char -> Parser ()
pExpect c = do
  x <- pChar
  guard $ x == c

pExpectPredicate :: (Char -> Bool) -> Parser Char
pExpectPredicate p = do
  x <- pChar
  P $ ES $ \(pos, s) -> if p x then Success (x :# (pos, s)) else Error $ ErrorAtPos pos

pOneUnsigned :: Parser Int
pOneUnsigned = do
  x <- pExpectPredicate Data.Char.isDigit
  pure (Data.Char.digitToInt x)

listToDouble :: [Int] -> Double
listToDouble [] = 0
listToDouble (x : xs) = fromIntegral x + 10 * listToDouble xs

listToSmallDouble :: [Int] -> Double
listToSmallDouble [] = 0
listToSmallDouble (x : xs) = fromIntegral x / 10 + listToSmallDouble xs / 10

pInt :: Parser Double
pInt = fmap (listToDouble . reverse) (some pOneUnsigned)

pDoublePoint :: Parser Double
pDoublePoint = do
  x <- pInt
  pExpect '.'
  y <- fmap listToSmallDouble (some pOneUnsigned)
  pure (x + y)

pDouble :: Parser Expr
pDouble = do
  pWs
  fmap Val (pDoublePoint <|> pInt)

pMulDiv :: Parser Expr
pMulDiv = pLevel pBracket charToMulDiv

charToMulDiv :: Char -> Maybe (Expr -> Expr -> Expr)
charToMulDiv '/' = Just (/)
charToMulDiv '*' = Just (*)
charToMulDiv _ = Nothing

charToSubAdd :: Char -> Maybe (Expr -> Expr -> Expr)
charToSubAdd '+' = Just (+)
charToSubAdd '-' = Just (-)
charToSubAdd _ = Nothing

pOper :: (Char -> Maybe (Expr -> Expr -> Expr)) -> Parser (Expr -> Expr -> Expr)
pOper conv = do
  pWs
  x <- fmap conv pChar
  maybe empty pure x

pLevel :: Parser Expr -> (Char -> Maybe (Expr -> Expr -> Expr)) -> Parser Expr
pLevel nextLevel charToOp = do
  x <- nextLevel
  li <-
    many
      ( do
          f <- pOper charToOp
          y <- nextLevel <|> pExpression
          pure (`f` y)
      )
  pure (foldl (\a f -> f a) x li)

pSubAdd :: Parser Expr
pSubAdd = pLevel pMulDiv charToSubAdd

pBracket :: Parser Expr
pBracket =
  ( do
      pWs
      pExpect '('
      x <- pExpression
      pWs
      pExpect ')'
      pure x
  ) <|> pDouble

pExpression :: Parser Expr
pExpression = pSubAdd

parseExpr :: String -> Except ParseError Expr
parseExpr = runP (do
  x <- pExpression
  pWs
  pEof
  pure x)