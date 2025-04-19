module Main (
    main
  ) where

import Lexer
import Grammar
import System.IO
import System.Environment
import Data.String.Utils




type TableContent = [[String]]
type Labels = [(String, Int)]
type ColumnData = [String]
type RowData = [String]

data Table = T TableContent Labels deriving (Read, Show)

type TableVariable = (String, Table)
type TableEnvironment = [TableVariable]

main :: IO()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            content <- readFile filename
            let tokens = alexScanTokens content
            let tree = parseCalc tokens
            eval tree

--The main evaluation function. 
--This function calls smaller evaluation functions to perform the following tasks:
  -- Read the input files into variables
  -- Perform queries on the input variables (tables), to possibly create new variables
  -- Output the final table
eval :: Start -> IO()
eval (Start inputs queries output) = do
                                      inputEnvironment <- evalInputs inputs
                                      let queryResultEnvironment = evalQueries queries inputEnvironment
                                      evalOutput output queryResultEnvironment

--Evaluates a list of inputs, 
-- and outputs a TableEnvironment containing the input tables, and possibly their labels as well
evalInputs :: Inputs -> IO TableEnvironment
evalInputs (InputSingle input) = do
                                  singleInput <- evalInputSingle input
                                  return [singleInput]
evalInputs (InputsCons input inputs) = do
                                        singleInput <- evalInputSingle input
                                        remainingInputs <- evalInputs inputs
                                        return (singleInput:remainingInputs)

--Evaluates a single input, and outputs a single assignment between a table name and a table 
evalInputSingle :: Input -> IO TableVariable
evalInputSingle (Input filename tableAssignment) = do
                                                fileContent <- readFile filename
                                                let tableContent = getTableContent fileContent
                                                let tableVariable = makeTableVariable tableAssignment tableContent
                                                return tableVariable

--Creates an association between a table and its name from a table assignment and table content
makeTableVariable :: TableAssignment -> TableContent -> TableVariable
makeTableVariable tableAssignment tableContent = (tableName, table)
  where
    (tableName,labels) = case tableAssignment of
                            NoLabels (TableRef name) -> (name, [])
                            WithLabels (TableRef name) columnLabels -> (name, evalColumnLabels columnLabels)
    table = T tableContent labels

--Converts a string in csv format into a table
getTableContent :: String -> TableContent
getTableContent fileContent = finalTable
  where
    stringRows = lines fileContent
    finalTable = map (split ",") stringRows

--Evaluates a list of column labels, re-representing them as strings
--Gives an error if the number of labels doesn't match the number of columns
evalColumnLabels :: ColumnLabels -> Labels
evalColumnLabels columnLabels = undefined

--Evaluates a list of queries on a given TableEnvironment, and ouptuts a new TableEnvironment with the results from the queries
evalQueries :: Queries -> TableEnvironment -> TableEnvironment
evalQueries queries tableEnvironment = case queries of
                                        QueryLet tableAssignment query remainingQueries -> 
                                                evalQueries remainingQueries newTableEnvironment
                                                where
                                                  newTable = makeTableVariable tableAssignment (evalQuery query tableEnvironment)
                                                  newTableEnvironment = 
                                                      updateTableEnvironment newTable tableEnvironment
                                                      
                                        QueryEnd -> tableEnvironment

--Adds a new TableVariable to a given TableEnvironment
--If the new table has the same name as one that already exists in the environment, it replaces the old one                                                                                  
updateTableEnvironment :: TableVariable -> TableEnvironment -> TableEnvironment
updateTableEnvironment tableVariable tableEnvironment = undefined

evalQuery :: Query -> TableEnvironment -> TableContent
evalQuery query tableEnvironment = undefined

--Evaluates an output expression using a table environment, and outputs a specified table to a specified output channel
-- The output channels supported are standard output and outputting to a file with a specified name
evalOutput :: Output -> TableEnvironment -> IO()
evalOutput (OutputConstruct (TableRef tableName) outputType) tableEnvironment = do
                                                                  let table = lookupTable tableName tableEnvironment
                                                                  let stringTable = tableToString table
                                                                  case outputType of
                                                                    Standard -> print stringTable
                                                                    File filename -> writeFile filename stringTable
                                                                  
--Reformats a table into a string
tableToString :: Table -> String
tableToString (T tablecontent _) = finalTable
  where
    rowStrings = map (join ",") tablecontent
    finalTable = unlines rowStrings


--Finds a table in a given TableEnvironment using the name of the table
--Gives an error if the key doesn't exist in the environment
lookupTable :: String -> TableEnvironment -> Table
lookupTable key environment = read stringTable
  where
    lookupOutput = lookup key environment
    stringTable = case lookupOutput of
            Nothing -> error "Variable Error: The table with the name'" ++ key ++ "' could not be found."
            Just table -> show table

--Finds the index of a column using the label of the column, given a list of associations between labels and indexes
--Gives an error if the label doesn't exist in the list
lookupLabel :: String -> Labels -> Int
lookupLabel key environment = read stringLabel
  where
    lookupOutput = lookup key environment
    stringLabel = case lookupOutput of
            Nothing -> error "Variable Error: The table with the name'" ++ key ++ "' could not be found."
            Just label -> show label

--Returns all columns of a given table given a column and a TableEnvironment
getColumn :: ColumnReference -> TableEnvironment -> ColumnData
getColumn reference environment = dataInColumn
  where
    (TableRef tableName, column, refIsLabel) = case reference of
                                  AlphaColumn name string -> (name, string, True)
                                  IntegerColumn name index -> (name, show index, False)
    T tableContent labels = lookupTable tableName environment
    index | refIsLabel = lookupLabel column labels
          | otherwise = read column

    dataInColumn = [value | row <- tableContent,
                      let value = row!!index]