import           Control.Monad         (replicateM)
import           Data.Char             (isSpace)
import           Data.Either           (isRight)
import           Data.List             (foldl')
import           Data.List             (dropWhileEnd)
import           Test.Hspec            (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary (..), Gen, choose, elements,
                                        oneof, (===))

import qualified Parser                as P
import qualified RPN                   as RPN
import           ShuntingYard          (ShuntingError (..), intoRPN)

main :: IO ()
main = hspec $
    describe "Calculator" $ do
        expressionParserSpec
        shuntingYardSpec

expressionParserSpec :: Spec
expressionParserSpec = do
    describe "Expression parser" $ do
        prop "It can parse arbitrary expression"
            (\(ExpressionStringGenerator expr _) ->
                isRight $ P.parseExpression expr)
        prop "The length of the token should be valid"
            (\(ExpressionStringGenerator expr num) ->
                fmap length (P.parseExpression expr) === Right num)

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

shuntingYardSpec :: Spec
shuntingYardSpec = describe "ShuntingYard" $ do
    it "Can convert simple expression `4 + 4` into RPN" $
        (intoRPN [P.Num 4, P.Plus, P.Num 10])
        `shouldBe`
        (Right [RPN.NUM 4, RPN.NUM 10, RPN.ADD])
    it "Can convert complex expression `5 + (1 + 2 * 4) ^ 2` into RPN" $
        (intoRPN [P.Num 5, P.Plus, P.OpenBracket, P.Num 1, P.Plus, P.Num 2, P.Multiply, P.Num 4, P.ClosingBracket, P.Exponent, P.Num 2])
        `shouldBe`
        (Right [RPN.NUM 5,RPN.NUM 1,RPN.NUM 2,RPN.NUM 4,RPN.MULTIPLY,RPN.ADD,RPN.NUM 2,RPN.EXPONENT,RPN.ADD])
    it "Will throw error on invalid bracket usage `(4 + 4`" $
        (intoRPN [P.OpenBracket, P.Num 4, P.Plus, P.Num 10])
        `shouldBe`
        (Left BracketLeftOnStack)
    it "Will throw error on invalid bracket usage `4 + 4)`" $
        (intoRPN [P.Num 4, P.Plus, P.Num 10, P.ClosingBracket])
        `shouldBe`
        (Left InvalidBracket)
    it "Will not throw error on invalid expression  4 *+ 4" $
        (intoRPN [P.Num 4, P.Multiply, P.Plus, P.Num 4])
        `shouldBe`
        (Right [RPN.NUM 4,RPN.MULTIPLY,RPN.NUM 4,RPN.ADD])
