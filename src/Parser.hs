module Parser where

import           Text.Parsec (ParseError, Parsec, anyChar, choice, digit,
                              getState, many1, modifyState, oneOf, optionMaybe,
                              parserFail, runParser, spaces, (<?>), (<|>))
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
    deriving (Show, Read, Eq)

parseExpression :: String -> Either ParseError [Token]
parseExpression input = runParser (exprParser >> getState) mempty "Expression parser" input

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
    str <- spaces *>  choice [many1 digit <|> (pure <$> operator)] <* spaces
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


