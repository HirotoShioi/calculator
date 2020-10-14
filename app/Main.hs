module Main where

import CLI
import Options.Applicative

main :: IO ()
main = do
    options <- execParser opts
    print options
