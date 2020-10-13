module Calculator
    ( calculate
    , CalculatorError
    ) where

-- パーサー、電卓アルゴリズムを組み合わせて、与えられた文字列から計算結果を算出する関数calculate
-- を実装するモジュール

import           Data.Char         (isSpace)
import           Data.List         (dropWhileEnd)
import qualified Parser            as P
import qualified RPN               as RPN
import           ShuntingYard      (intoRPN, renderShuntingYardError)
import           Text.Parsec.Error

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

type CalculatorError = String

calculate :: String -> Either CalculatorError Int
calculate input = do
    parsedData <- parseErrorToString $ P.parseString input
    rpn <-  errorToString renderShuntingYardError $ intoRPN parsedData
    errorToString RPN.renderRPNError $ RPN.evaluate rpn

errorToString :: (Show a) => (a -> String) -> Either a b -> Either String b
errorToString showFunc (Left e)     = (Left . showFunc) e
errorToString _showFunc (Right res) = Right res

parseErrorToString :: Either ParseError b -> Either String b
parseErrorToString (Left parseError) = (Left . strip . unlines . fmap messageString . errorMessages) parseError
parseErrorToString (Right res) = Right res
