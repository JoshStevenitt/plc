module Main (
    main
  ) where

import Lib (introMessage)
import Lexer
import Grammar
import System.IO
import System.Environment

main :: IO()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let tokens = alexScanTokens content
            print ""
            print ""
            print ""
            print tokens
            print ""
            print ""
            print ""
            let tree = parseCalc tokens
            print tree
            print ""
            print ""
