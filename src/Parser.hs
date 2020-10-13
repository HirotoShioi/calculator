{-# LANGUAGE LambdaCase #-}

module Parser
    ( Token(..)
    , parseString
    ) where

import           Text.Parsec (ParseError, Parsec, anyChar, char, choice, digit,
                              getState, many1, modifyState, oneOf, optionMaybe,
                              parserFail, runParser, spaces, try, (<?>), (<|>))
import           Text.Read   (readMaybe)

-- 式5 + 6 * (7 + 8) を [Num 5, Num 6, Multiply, OpenBracket, Num 7, Plus, Num 8, ClosingBracket]
-- という風にトークンとして解析する

-- | トークン
data Token
    = Num Int -- ^ 整数
    | Plus -- ^ 演算子 "+"
    | Minus -- ^ 演算子 "-"
    | Multiply -- ^ 演算子 "*"
    | Divide -- ^ 演算子 "/"
    | Exponent　-- ^ 演算子 "^"
    | OpenBracket -- ^ 開け括弧
    | ClosingBracket -- ^ 閉じ括弧
    deriving (Show, Read, Eq, Ord)

-- | 与えられた数式文字列を解析する
parseString :: String -> Either ParseError [Token]
parseString input = runParser (exprParser >> getState) mempty "Expression parser" input

-- | トークンをパースする
exprParser :: Parsec String [Token] ()
exprParser = do
    mToken <- optionMaybe parseToken
    case mToken of
        Nothing -> do
            -- パースできない文字があるか確認
            mChar <- optionMaybe anyChar
            case mChar of
                Nothing -> return () -- 何もない、解析完了
                -- パースできない文字列があった、エラーを投げる
                Just c  -> parserFail $ "Unepxected character: " <> [c]
        -- トークンがパースされた、FILOスタックに追加する
        -- 追加後、再度トークンを解析する
        Just t -> modifyState (\ls -> ls ++ [t]) >> exprParser

-- |　数及び演算子をパースする
parseToken :: Parsec String [Token] Token
parseToken = spaces *> parser <* spaces
  where
    parser :: Parsec String [Token] Token
    parser = choice [try parseNegativeNumber <|> parsePositiveNumber <|> parseOperator]

-- | 正の数をパースする
parsePositiveNumber :: Parsec String state Token
parsePositiveNumber = do
    numStr <- many1 digit
    case readMaybe numStr of
        Nothing  -> fail $ "Not a number: " <> numStr
        Just num -> return $ Num num

-- | 演算子をパースする
parseOperator :: Parsec String state Token
parseOperator = do
    op <- oneOf "+-*/^()" <?> "operator"
    case op of
        '+'    -> return Plus
        '-'    -> return Minus
        '*'    -> return Multiply
        '/'    -> return Divide
        '^'    -> return Exponent
        '('    -> return OpenBracket
        ')'    -> return ClosingBracket
        others -> fail $ "Unexpected token: " <> [others]

-- | 負の数をパースする
--
-- [Token]のlastが数字ならなにもしない、もし演算子ならパースする
parseNegativeNumber :: Parsec String [Token] Token
parseNegativeNumber = do
    ls <- getState
    if null ls
        then parseNegative
        else if isOperator (last ls)
            then parseNegative
            else fail "Probably operator, move on"
  where
    parseNegative :: Parsec String [Token] Token
    parseNegative = do
        num <- char '-' *> many1 digit
        case readMaybe num of
            Nothing -> fail $ "Not an number: " <> num
            Just n  -> return $ Num $ negate n
    isOperator :: Token -> Bool
    isOperator = \case
        Num _ -> False
        _ -> True

