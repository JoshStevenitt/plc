module Main (
    main
  ) where

import Lexer
import Grammar
import System.Environment






main :: IO()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let tokens = alexScanTokens content
            print tokens
            let tree = parseCalc tokens
            print tree

