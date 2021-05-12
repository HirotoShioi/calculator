module Main where

import           Calculator          (calculate)
import           CLI                 (Input (..), opts)
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Options.Applicative (execParser)

main :: IO ()
main = do
    opt <- execParser opts
    case opt of
        Expression expr -> calculateAndPrint expr
        FileInput path -> do
            exprs <- T.lines <$> T.readFile (T.unpack path)
            mapM_ calculateAndPrint exprs
  where
    calculateAndPrint :: Text -> IO ()
    calculateAndPrint expr = do
        case calculate (T.unpack expr) of
            Right res -> putStrLn $ show res
            Left err  -> putStrLn $ "Error: " <> err
