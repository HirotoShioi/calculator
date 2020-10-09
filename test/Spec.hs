import           Control.Monad         (replicateM)
import           Data.Char             (isSpace)
import           Data.Either           (isRight)
import           Data.List             (foldl')
import           Data.List             (dropWhileEnd)
import           Test.Hspec            (describe, hspec)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, choose, elements,
                                        oneof, (===))

import           Parser                (parseExpression)

main :: IO ()
main = hspec $
    describe "Expression parser" $ do
        prop "It can parse arbitrary expression"
            (\(ExpressionStringGenerator expr _) -> isRight $ parseExpression expr)
        prop "The length of the token should be valid"
            (\(ExpressionStringGenerator expr num) -> fmap length (parseExpression expr) === Right num)

data ExpressionStringGenerator = ExpressionStringGenerator String Int
  deriving Show

instance Arbitrary ExpressionStringGenerator where
    arbitrary = do
        exprLength <- choose (1, 20)
        expressions <- replicateM exprLength (oneof [genPositiveNumStr, genOperator])
        let expressionStr = strip $ foldl' (\acc expr -> acc <> " " <> expr) mempty expressions
        return $ ExpressionStringGenerator expressionStr exprLength
        where
        -- Only positive numbers
        genPositiveNumStr :: Gen String
        genPositiveNumStr = show <$> (choose (1, 100) :: Gen Int)
        genOperator :: Gen String
        genOperator = return <$> elements "+-/*^()"

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace
