{-# LANGUAGE LambdaCase #-}

module RPN
    ( Token(..)
    , evaluate
    , EvaluationError(..)
    , renderRPNError
    )
    where

import           Control.Monad.Except (Except, runExcept, throwError)
import           Control.Monad.State  (StateT, evalStateT, get, modify')

-- 逆ポーランド記法を用いて計算式を解く

data Token
    = NUM Int
    | ADD
    | SUBTRACT
    | MULTIPLY
    | DIVIDE
    | EXPONENT
    deriving (Show, Read, Eq, Ord)

data EvaluationError
    = DivideByZero -- ^ 0 で割ろうとした
    | NegativeExponent Int -- ^ 負の数で累乗した
    | EmptyStack -- ^ スタックが空
    | SecondNumMissing [Int] -- ^ 2つ目の数字がない
    | TooManyOnStack [Int] -- ^ 計算結果を求めた際にスタックに2つ以上の値が存在した
    deriving (Eq, Show, Read)

renderRPNError :: RPN.EvaluationError -> String
renderRPNError = \case
    DivideByZero -> "Divided by zero"
    NegativeExponent n -> "Power is negative: " <> show n
    EmptyStack -> "Stack is empty"
    SecondNumMissing ls -> "Second number could not be obtained: " <> show ls
    TooManyOnStack ls -> "More than two number on stack: " <> show ls

type Evaluator a = StateT [Int] (Except EvaluationError) a

evaluate :: [Token] -> Either EvaluationError Int
evaluate tokens = (runExcept . flip evalStateT mempty . run) tokens

run :: [Token] -> Evaluator Int
run tokens = do
    mapM_ f tokens
    ls <- get
    if null ls
        then throwError EmptyStack
        else if length ls > 1
            then throwError $ TooManyOnStack ls
            else return $ head ls
  where
    f :: Token -> Evaluator ()
    f = \case
       NUM num -> modify' (\ls -> num : ls)
       ADD -> compute (+)
       SUBTRACT -> compute (-)
       MULTIPLY -> compute (*)
       EXPONENT -> do
           e <- getFirstVal
           if e < 0
               then throwError $ NegativeExponent e
               else do
                   val <- getSecondVal
                   modify' (\stack -> val ^ e : drop 2 stack)
       DIVIDE -> do
           denominator <- getFirstVal
           if denominator == 0
               then throwError DivideByZero
               else do
                   val <- getSecondVal
                   modify' (\stack -> val `div` denominator : drop 2 stack)
    compute :: (Int -> Int -> Int) -> Evaluator ()
    compute g = do
        val1 <- getFirstVal
        val2 <- getSecondVal
        modify' (\stack -> g val2 val1 : drop 2 stack)

getFirstVal :: Evaluator Int
getFirstVal = do
    stack <- get
    if null stack
        then throwError EmptyStack
        else return $ head stack

getSecondVal :: Evaluator Int
getSecondVal = do
    stack <- get
    if length stack < 2
        then throwError $ SecondNumMissing stack
        else return $ head $ drop 1 stack
