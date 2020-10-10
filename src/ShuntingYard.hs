{-# LANGUAGE LambdaCase #-}

module ShuntingYard
    ( intoRPN
    , ShuntingError(..)
    ) where

import           Control.Monad.Except (Except, runExcept, throwError)
import           Control.Monad.State  (StateT, execStateT, get, modify')
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromJust)
import qualified Parser               as P
import qualified RPN                  as RPN

data EvalStack =  EvalStack {
      stack :: ![P.Token]
    , queue :: ![RPN.Token]
    } deriving Show

data ShuntingError
    = InvalidBracket
    | UnexpectedToken P.Token
    | StackEmpty
    | BracketLeftOnStack
    | PushingBracketToQueue P.Token
    deriving Show

type Evaluator a = StateT EvalStack (Except ShuntingError) a

-- data Operator = Add | Subtract | Multiply | Divide | Exponent | OpenBracket | ClosingBracket deriving Show

initState :: EvalStack
initState = EvalStack mempty mempty

intoRPN :: [P.Token] -> Either ShuntingError [RPN.Token]
intoRPN tokens = queue <$> (runExcept . flip execStateT initState . run) tokens

run :: [P.Token] -> Evaluator ()
run tokens = do
    mapM_ eval tokens
    pushToQueueUntilEmpty -- ここでスタックにある全ての演算子をキューに追加する
  where
    pushToQueueUntilEmpty :: Evaluator ()
    pushToQueueUntilEmpty = do
        s <- stack <$> get
        if any (`elem` [P.OpenBracket, P.ClosingBracket]) s
            then throwError BracketLeftOnStack
            else do
                mOp <- peekStack
                case mOp of
                    Nothing -> return ()
                    Just _  -> stackToQueue >> pushToQueueUntilEmpty


eval :: P.Token -> Evaluator ()
eval token = case token of
    P.Num n          -> pushToQueue (P.Num n)
    P.OpenBracket    -> pushToStack P.OpenBracket
    P.ClosingBracket -> pushToQueueUntilOpenBracket
    others           -> evalOperator others
  where
    pushToQueueUntilOpenBracket :: Evaluator ()
    pushToQueueUntilOpenBracket = do
        mOp <- peekStack
        case mOp of
            Nothing -> throwError InvalidBracket
            Just op ->  case op of
                P.Num n       -> throwError $ UnexpectedToken (P.Num n)
                P.OpenBracket -> popStack >> return () -- 左括弧を捨てる
                _others       -> stackToQueue >> pushToQueueUntilOpenBracket
    evalOperator :: P.Token -> Evaluator ()
    evalOperator op1 = do
        mOp <- peekStack
        case mOp of
            Nothing -> pushToStack op1
            Just op2 -> do
                if (compareOperators (<=) op1 op2 && isLeftAssociative op1) || compareOperators (<) op1 op2
                then stackToQueue >> eval token
                else pushToStack op1

--------------------------------------------------------------------------------
-- 補助関数
--------------------------------------------------------------------------------

pushToQueue :: P.Token -> Evaluator ()
pushToQueue token = do
    rpnToken <- toRPNToken token
    modify' (\s -> s { queue = (queue s) ++ [rpnToken] })
  where
    toRPNToken :: P.Token -> Evaluator RPN.Token
    toRPNToken = \case
      P.Num n -> return $ RPN.NUM n
      P.Plus -> return RPN.ADD
      P.Minus -> return RPN.SUBTRACT
      P.Multiply -> return RPN.MULTIPLY
      P.Divide -> return RPN.DIVIDE
      P.Exponent -> return RPN.EXPONENT
      others -> throwError $ PushingBracketToQueue others

pushToStack :: P.Token -> Evaluator ()
pushToStack = \case
    P.Num n -> throwError $ UnexpectedToken (P.Num n)
    others -> modify' (\s -> s { stack = others : (stack s)})

peekStack :: Evaluator (Maybe P.Token)
peekStack = do
    s <- stack <$> get
    if null s
        then return Nothing
        else return $ Just $ head s

popStack :: Evaluator P.Token
popStack = do
    s <- stack <$> get
    if null s
        then throwError StackEmpty
        else do
            modify' (\st -> st { stack = tail s})
            return $ head s

stackToQueue :: Evaluator ()
stackToQueue = do
    operator <- popStack
    pushToQueue operator

--------------------------------------------------------------------------------
-- 演算子の評価に用いる関数郡
--------------------------------------------------------------------------------

isRightAssociative :: P.Token -> Bool
isRightAssociative = \case
    P.Exponent -> True
    _others -> False

isLeftAssociative :: P.Token -> Bool
isLeftAssociative = not . isRightAssociative

compareOperators :: (Int -> Int -> Bool) -> P.Token -> P.Token -> Bool
compareOperators comp op1 op2 =
    let p1 = getPriority op1 operatorPriorityList
        p2 = getPriority op2 operatorPriorityList
    in comp p1 p2
  where
    getPriority :: P.Token -> M.Map P.Token Int -> Int
    getPriority token m = fromJust $ M.lookup token m
    operatorPriorityList :: M.Map P.Token Int
    operatorPriorityList = M.fromList
        [ (P.Exponent, 4)
        , (P.Multiply, 3)
        , (P.Divide, 3)
        , (P.Plus, 2)
        , (P.Minus, 2)
        , (P.OpenBracket, -1)
        , (P.ClosingBracket, -1)
        ]
