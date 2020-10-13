{-# LANGUAGE LambdaCase #-}

module Parser
    ( Token(..)
    , parseString
    ) where

import           Text.Parsec (ParseError, Parsec, anyChar, choice, digit,
                              getState, many1, modifyState, oneOf, optionMaybe,
                              parserFail, runParser, spaces, (<?>), (<|>), char, try)
import           Text.Read   (readMaybe)

-- 式5 + 6 * (7 + 8) を [Num 5, Num 6, Multiply, OpenBracket, Num 7, Plus, Num 8, ClosingBracket]
-- という風にトークンとして解析する

data Token
    = Num Int
    | Plus
    | Minus
    | Multiply
    | Divide
    | Exponent
    | OpenBracket
    | ClosingBracket
    deriving (Show, Read, Eq, Ord)

parseString :: String -> Either ParseError [Token]
parseString input = runParser (exprParser >> getState) mempty "Expression parser" input

-- 14 + 4
exprParser :: Parsec String [Token] ()
exprParser = do
    mToken <- optionMaybe parseToken
    case mToken of
        Nothing -> do
            -- 無効なキャラクター
            mChar <- optionMaybe anyChar
            case mChar of
                Nothing -> return () -- Parse complete
                Just c  -> parserFail $ "Unepxected character: " <> [c] -- Throw error
        -- Put the result into the state, keep going
        -- We want this to be in FILO
        Just t -> modifyState (\ls -> ls ++ [t]) >> exprParser

parseToken :: Parsec String [Token] Token
parseToken = do
    str <- spaces *>  choice [try parseNegativeNumber <|> many1 digit <|> (pure <$> operator)] <* spaces
    case readMaybe str of
        Just n -> return $ Num n
        Nothing -> case str of
            "+"    -> return Plus
            "-"    -> return Minus
            "*"    -> return Multiply
            "/"    -> return Divide
            "^"    -> return Exponent
            "("    -> return OpenBracket
            ")"    -> return ClosingBracket
            others -> fail $ "Unexpected token: " <> others

operator :: Parsec String state Char
operator = oneOf "+-*/^()" <?> "operator"

-- [Token]のlastが数字ならなにもしない、もし演算子ならパースする
parseNegativeNumber :: Parsec String [Token] String
parseNegativeNumber = do
    ls <- getState
    if null ls
        then parseNegative
        else if isOperator (last ls)
            then parseNegative
            else parserFail "Probably operator, move on"
  where
    parseNegative :: Parsec String [Token] String
    parseNegative = do
        minus <- char '-'
        num <- many1 digit
        return $ minus : num
    isOperator :: Token -> Bool
    isOperator = \case
        Num _ -> False
        _ -> True

