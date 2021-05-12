module CLI
  ( opts
  , Input(..)
  ) where

import           Data.Text           (Text)
import           Options.Applicative (Parser, ParserInfo, fullDesc, header,
                                      help, helper, info, long, metavar,
                                      progDesc, short, strArgument, strOption,
                                      (<**>), (<|>))

-- コマンドラインの挙動を実装するモジュール

data Input
    = Expression Text
    | FileInput Text
    deriving Show

fileInput :: Parser Input
fileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "Input file" )

stdInput :: Parser Input
stdInput = Expression <$> strArgument
  (  help "Read math expression from CLI" )


input :: Parser Input
input = fileInput <|> stdInput

opts :: ParserInfo Input
opts = info (input <**> helper)
  ( fullDesc
  <> progDesc "Calculator"
  <> header "Calculator implemented in haskell" )
