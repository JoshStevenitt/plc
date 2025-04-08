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
            let tree = parseCalc $ alexScanTokens content
            print tree