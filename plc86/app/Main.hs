{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Use elemIndex" #-}
module Main (
    main
  ) where

import Lexer
import Grammar
import System.IO
import System.Environment
import Data.String.Utils
import Data.List





type TableContent = [RowData]
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
                                        return (updateTableEnvironment singleInput remainingInputs)

--Evaluates a single input, and outputs a single assignment between a table name and a table 
evalInputSingle :: Input -> IO TableVariable
evalInputSingle (Input filename tableAssignment) = do
                                                fileContent <- readFile filename
                                                let tableContent = getTableContent fileContent
                                                let tableVariable = makeTableVariable tableAssignment tableContent
                                                return tableVariable

--Creates an association between a table and its name from a table assignment and table content
--Gives an error if the number of labels doesn't match the number of columns
--Gives an error if the number of columns doesn't match for every row
makeTableVariable :: TableAssignment -> TableContent -> TableVariable
makeTableVariable tableAssignment tableContent = (tableName, read tableString)
  where
    strippedTableContent = [[strip item | item <- row] | row <- tableContent]
    (tableName,labels) = case tableAssignment of
                            NoLabels (TableRef name) -> (name, [])
                            WithLabels (TableRef name) (LabelConstructor columnLabels) -> (name, evalColumnLabels columnLabels)
    tableString | not (all ((== length (head tableContent)) . length) tableContent)
                = error ("The number of columns in the table '" ++ tableName ++ "' are not the same for all rows.")
          | (not $ null labels) && (not (length (head tableContent) == length labels))
                = error ("The number of labels does not match the number of columns for table '" ++ tableName ++ "'")
          | nub labels /= labels
                = error "There are multiple labels with the same name in the table, this is not allowed."
          | otherwise = show (T strippedTableContent labels)

--Returns the tablename given by a table variable
tableAssignmentToTableName :: TableAssignment -> TableName
tableAssignmentToTableName (NoLabels tableName) = tableName
tableAssignmentToTableName (WithLabels tableName _) = tableName

--Converts a string in csv format into a table
getTableContent :: String -> TableContent
getTableContent fileContent = finalTable
  where
    stringRows = lines fileContent
    finalTable = map (customSplit ",") stringRows

customSplit :: Eq a => [a] -> [a] -> [[a]]
customSplit _ [] = [[]]
customSplit delimiter list = split delimiter list

--Evaluates a list of column labels, re-representing them as strings
evalColumnLabels :: ColumnLabels2 -> Labels
evalColumnLabels columnLabels = evalColumnLabelsHelp columnLabels 1
  where
    evalColumnLabelsHelp (LabelSingular label) x = [(label, x)]
    evalColumnLabelsHelp (LabelsMultiple label remainingColumnLabels) x = (label, x):(evalColumnLabelsHelp remainingColumnLabels (x+1))

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
updateTableEnvironment tableVariable tableEnvironment | newTableName `elem` (map fst tableEnvironment) = [ (name, outputTable) | (name, table) <- tableEnvironment,
                                                                                                          let outputTable | name == newTableName = newTable
                                                                                                                          | otherwise = table]
                                                      | otherwise = tableVariable:tableEnvironment
  where
    (newTableName,newTable) = tableVariable

--Evaluates a single query.
--Matches the query given to the relevant evaluation function, and returns the resulting table
evalQuery :: Query -> TableEnvironment -> TableContent
evalQuery query tableEnvironment = case query of
                                    Merge tableName1 tableName2 mergeConstraint -> evalMerge tableName1 tableName2 mergeConstraint tableEnvironment
                                    SelecterQuery selection -> evalSelect selection tableEnvironment
                                    Product tableName1 tableName2 -> evalProduct tableName1 tableName2 tableEnvironment
                                    Sort tableName sortClause -> evalSort tableName sortClause tableEnvironment
                                    Insert string tableName position -> evalInsert string tableName position tableEnvironment
                                    Fill string tableName axis -> evalFill string tableName axis tableEnvironment
                                    Delete tableName axis -> evalDelete tableName axis tableEnvironment
                                    Clear tableName position -> evalClear tableName position tableEnvironment
                                    AddBlank tableName axis -> evalAddBlank tableName axis tableEnvironment

evalMerge :: TableName -> TableName -> BooleanExpression -> TableEnvironment -> TableContent
evalMerge tableName1 tableName2 boolExpr tableEnvironment = newTableContent
  where
    T tableContent1 _ = lookupTable tableName1 tableEnvironment
    T tableContent2 _ = lookupTable tableName2 tableEnvironment
    newTableContent | length (head tableContent1) /= length (head tableContent2) = error "evalMerge: the input tables have different arities"
                    | otherwise = [newRow |
                                    index1 <- [1 .. (length tableContent1)],
                                    let row1 = tableContent1 !! (index1 - 1),
                                    index2 <- [1 .. (length tableContent2)],
                                    let row2 = tableContent2 !! (index2 - 1),
                                    let newRow = getMergedRow row1 row2,
                                    matchRowListToExpression
                                      [row1, row2] [index1,index2] boolExpr [tableName1, tableName2] tableEnvironment]

    getMergedRow row1 row2 = [ item | (item1, item2) <- zip row1 row2,
                                                        let item | null item1 = item2
                                                                 | otherwise = item1]

matchRowListToExpression :: [RowData] -> [Int] -> BooleanExpression -> [TableName] -> TableEnvironment -> Bool
matchRowListToExpression rows indices boolExpr tableNames tableEnvironment = matchRToBoolHelp boolExpr
  where
    matchRToBoolHelp :: BooleanExpression -> Bool
    matchRToBoolHelp (BooleanBracket expression) = matchRToBoolHelp expression
    matchRToBoolHelp (BooleanAND expression1 expression2)
       = (matchRToBoolHelp expression1) && (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanOR expression1 expression2)
       = (matchRToBoolHelp expression1 ) || (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanNOT expression)  = not (matchRToBoolHelp expression)
    matchRToBoolHelp (BooleanEQ expression1 expression2)
       = (matchRToBoolHelp expression1) == (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanSubExpression se1 se2)
       = matchRToSubHelp se1 == matchRToSubHelp se2
    matchRToBoolHelp (BooleanIndexExpression ie1 ie2)
       = and [matchRToIndexHelp ie1 tableName == matchRToIndexHelp ie2 tableName | tableName <- tableNames]


    matchRToSubHelp (SubString string) = string
    matchRToSubHelp (SubColumn colRef) = correctRow!!(getLabelIndex colRef tableEnvironment -1)
      where
      refTableName = case colRef of
                      AlphaColumn tn _ -> tn
                      IntegerColumn tn _ -> tn
      correctRow = case elemIndex refTableName tableNames of
                      Just index -> rows!!index
                      Nothing -> error "The table you are referencing is not accessible here"


    matchRToIndexHelp :: IndexExpression -> TableName -> Int
    matchRToIndexHelp IndexSingular tableName = case elemIndex tableName tableNames of
                                        Just index -> indices!!index
                                        Nothing -> error "The table you are referencing is not accessible here"
    matchRToIndexHelp (IndexNum number) _ = case number of
                                            PositiveNumber num -> read num
                                            NegativeNumber num -> read num
    matchRToIndexHelp (IndexBracket ie) tableName = matchRToIndexHelp ie tableName
    matchRToIndexHelp (IndexPlus ie1 ie2) tableName = matchRToIndexHelp ie1 tableName + matchRToIndexHelp ie2 tableName
    matchRToIndexHelp (IndexMinus ie1 ie2) tableName = matchRToIndexHelp ie1 tableName - matchRToIndexHelp ie2 tableName
    matchRToIndexHelp (IndexMult ie1 ie2) tableName = matchRToIndexHelp ie1 tableName * matchRToIndexHelp ie2 tableName
    matchRToIndexHelp (IndexIntDiv ie1 ie2) tableName = matchRToIndexHelp ie1 tableName `div` matchRToIndexHelp ie2 tableName
      --Getting the remainder of a fractional division, similar to mod
    matchRToIndexHelp (IndexMod ie1 ie2) tableName = matchRToIndexHelp ie1 tableName `mod` matchRToIndexHelp ie2 tableName
    matchRToIndexHelp (IndexExpo ie1 ie2) tableName = matchRToIndexHelp ie1 tableName ^ matchRToIndexHelp ie2 tableName


evalSelect :: Selection -> TableEnvironment -> TableContent
evalSelect selection tableEnvironment = finalTable
  where
    Selecter maxClause distinct columnChoice tableExpression whereClause plusClause = selection
    (tableAfterFromClause, fromTableEnvironment) = (evalTableExpression tableExpression tableEnvironment)
    (tableAfterWhereClause, whereTableEnvironment) = evalWhereClause whereClause tableAfterFromClause fromTableEnvironment
    (tableAfterColumnChoice, columnTableEnvironment) = evalColumnChoice columnChoice tableAfterWhereClause whereTableEnvironment
    T whereClauseTableContent _ = lookupTable tableAfterColumnChoice columnTableEnvironment
    tableAfterDistinctClause = evalDistinctClause distinct whereClauseTableContent
    tableAfterMaxClause = evalMaxClause maxClause tableAfterDistinctClause
    finalTable = case plusClause of
                  PlusTrue nextSelection ->
                    tableAfterMaxClause ++ (evalSelect nextSelection whereTableEnvironment)
                  PlusFalse -> tableAfterMaxClause

evalTableExpression :: TableExpression -> TableEnvironment -> (TableName, TableEnvironment)
evalTableExpression tableExpression tableEnvironment = case tableExpression of
                                                          SingleTable tableName -> (tableName, tableEnvironment)
                                                          TableLetQuery tableAssignment query -> (tableAssignmentToTableName tableAssignment, newTableEnvironment)
                                                            where
                                                              newTable = makeTableVariable tableAssignment (evalQuery query tableEnvironment)
                                                              newTableEnvironment = updateTableEnvironment newTable tableEnvironment
                                                          TableLetJoin tableAssignment joinClause -> (tableAssignmentToTableName tableAssignment, newTableEnvironment)
                                                            where
                                                              newTable = makeTableVariable tableAssignment (evalJoin joinClause tableEnvironment)
                                                              newTableEnvironment = updateTableEnvironment newTable tableEnvironment
                                                          SingleTableExpression singleTableExpression -> evalTableExpression singleTableExpression tableEnvironment


evalWhereClause :: WhereClause -> TableName -> TableEnvironment -> (TableName, TableEnvironment)
evalWhereClause (WhereTrue booleanExpression) tableName tableEnvironment = (tableName, newTableEnvironment)
  where
    TableRef name = tableName
    T tableContent tableLabels = lookupTable tableName tableEnvironment
    newTableContent = [ row | index <- [1..(length tableContent)],
                          let row = tableContent!!(index-1),
                          matchRowToExpression row index booleanExpression tableName tableEnvironment]
    newTable = T newTableContent tableLabels
    newTableEnvironment = updateTableEnvironment (name, newTable) tableEnvironment

evalWhereClause WhereFalse tableName tableEnvironment = (tableName, tableEnvironment)

matchRowToExpression :: RowData -> Int -> BooleanExpression -> TableName -> TableEnvironment -> Bool
matchRowToExpression row rowIndex booleanExpression tableName tableEnvironment = matchRToBoolHelp booleanExpression
  where
    matchRToBoolHelp :: BooleanExpression -> Bool
    matchRToBoolHelp (BooleanBracket expression) = matchRToBoolHelp expression
    matchRToBoolHelp (BooleanAND expression1 expression2)
       = (matchRToBoolHelp expression1) && (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanOR expression1 expression2)
       = (matchRToBoolHelp expression1 ) || (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanNOT expression)  = not (matchRToBoolHelp expression)
    matchRToBoolHelp (BooleanEQ expression1 expression2)
       = (matchRToBoolHelp expression1) == (matchRToBoolHelp expression2)
    matchRToBoolHelp (BooleanSubExpression se1 se2)
       = matchRToSubHelp se1 == matchRToSubHelp se2
    matchRToBoolHelp (BooleanIndexExpression ie1 ie2)
       = matchRToIndexHelp ie1 == matchRToIndexHelp ie2


    matchRToSubHelp (SubString string) = string
    matchRToSubHelp (SubColumn colRef) | tableName /= refTableName = error "The table you are referencing in the where clause is not the same as the one produced in the from clause"
                                       | otherwise = row!!(getLabelIndex colRef tableEnvironment -1)
      where
      refTableName = case colRef of
                      AlphaColumn tn _ -> tn
                      IntegerColumn tn _ -> tn


    matchRToIndexHelp :: IndexExpression -> Int
    matchRToIndexHelp IndexSingular = rowIndex
    matchRToIndexHelp (IndexNum number) = case number of
                                            PositiveNumber num -> read num
                                            NegativeNumber num -> read num
    matchRToIndexHelp (IndexBracket ie) = matchRToIndexHelp ie
    matchRToIndexHelp (IndexPlus ie1 ie2) = matchRToIndexHelp ie1 + matchRToIndexHelp ie2
    matchRToIndexHelp (IndexMinus ie1 ie2) = matchRToIndexHelp ie1 - matchRToIndexHelp ie2
    matchRToIndexHelp (IndexMult ie1 ie2) = matchRToIndexHelp ie1 * matchRToIndexHelp ie2
    matchRToIndexHelp (IndexIntDiv ie1 ie2) = matchRToIndexHelp ie1 `div` matchRToIndexHelp ie2
      --Getting the remainder of a fractional division, similar to mod
    matchRToIndexHelp (IndexMod ie1 ie2) = matchRToIndexHelp ie1 `mod` matchRToIndexHelp ie2
    matchRToIndexHelp (IndexExpo ie1 ie2) = matchRToIndexHelp ie1 ^ matchRToIndexHelp ie2




evalColumnChoice :: ColumnChoice -> TableName -> TableEnvironment -> (TableName, TableEnvironment)
evalColumnChoice ColumnALL tableName tableEnvironment = (tableName, tableEnvironment)
evalColumnChoice columnChoice tableName tableEnvironment = (tableName, newTableEnvironment)
  where
    TableRef name = tableName
    newTableContent = columnChoiceHelp columnChoice []
    newTable = T newTableContent []
    newTableEnvironment = updateTableEnvironment (name, newTable) tableEnvironment

    columnChoiceHelp (ColumnSingle colRef) x = transpose (reverse (getColumn colRef tableEnvironment:x))
    columnChoiceHelp (ColumnMultiple colRef remainingColumnChoice) x = columnChoiceHelp remainingColumnChoice (getColumn colRef tableEnvironment:x)



evalDistinctClause :: Distinct -> TableContent -> TableContent
evalDistinctClause DistinctTrue tableContent = nub tableContent
evalDistinctClause DistinctFalse tableContent = tableContent

evalMaxClause :: MaxClause -> TableContent -> TableContent
evalMaxClause (MaxTrue (PositiveNumber num)) tableContent = take (read num) tableContent
evalMaxClause (MaxTrue _) _ = error "evalMaxClause: input number cannot be negative"
evalMaxClause MaxFalse tableContent = tableContent

evalJoin :: JoinClause -> TableEnvironment -> TableContent
evalJoin (JoinClause leftTableExpression joinOperator rightTableExpression boolExpr) tableEnv
     = case joinOperator of
          JoinInner -> [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex rightIndex,
                          matchRowListToExpression
                            [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv]

          JoinLeft -> [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex rightIndex,
                          matchRowListToExpression
                            [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv]

                  ++ [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex 0,
                          not (or [matchRowListToExpression
                                    [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv |
                                    rightIndex <- [1 .. (length rightTableContent)],
                                    let rightRow = rightTableContent !! (rightIndex - 1)])]
          JoinRight -> [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex rightIndex,
                          matchRowListToExpression
                            [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv]

                  ++ [newRow |
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow 0 rightIndex,
                          not (or [matchRowListToExpression
                                     [leftRow, rightRow] [leftIndex, rightIndex] boolExpr
                                     [leftTableName, rightTableName] finalTableEnv |
                                     let leftRow = rightTableContent !! (rightIndex - 1),
                                     leftIndex <- [1 .. (length rightTableContent)]])]
          JoinFull -> [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex rightIndex,
                          matchRowListToExpression
                            [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv]

                  ++ [newRow |
                          leftIndex <- [1 .. (length leftTableContent)],
                          let leftRow = leftTableContent !! (leftIndex - 1),
                          let (newRow,_) = createJoinedRow leftIndex 0,
                          not (or [matchRowListToExpression
                                    [leftRow, rightRow] [leftIndex,rightIndex] boolExpr [leftTableName, rightTableName] finalTableEnv |
                                    rightIndex <- [1 .. (length rightTableContent)],
                                    let rightRow = rightTableContent !! (rightIndex - 1)])]

                  ++ [newRow |
                          rightIndex <- [1 .. (length rightTableContent)],
                          let rightRow = rightTableContent !! (rightIndex - 1),
                          let (newRow,_) = createJoinedRow 0 rightIndex,
                          not (or [matchRowListToExpression
                                     [leftRow, rightRow] [leftIndex, rightIndex] boolExpr
                                     [leftTableName, rightTableName] finalTableEnv |
                                     let leftRow = rightTableContent !! (rightIndex - 1),
                                     leftIndex <- [1 .. (length rightTableContent)]])]


  where
    (leftTableName, leftTableEnv) = evalTableExpression leftTableExpression tableEnv
    (rightTableName, finalTableEnv) = evalTableExpression rightTableExpression leftTableEnv
    T leftTableContent _ = lookupTable leftTableName finalTableEnv
    T rightTableContent _ = lookupTable rightTableName finalTableEnv
    rightArity = length (head rightTableContent)

    --Given two rows, it creates a joined row based on the boolean expression given
    --It also returns whether or not the rows match the boolean expression given
    createJoinedRow :: Int -> Int -> (RowData,Bool)
    createJoinedRow leftIndex rightIndex = (joinedRow, match)
      where
        leftRow = leftTableContent!!leftIndex
        rightRow = rightTableContent!!rightIndex

        joinColumns = getJoinColumns boolExpr
        (joinedRow, match) = case joinColumns of
                                Just (leftCol, rightCol) | leftRow!!leftCol == rightRow!!rightCol -> (leftRow ++ removeAt rightCol rightRow, True)
                                                         | otherwise -> (leftRow ++ replicate (rightArity-1) "",False)
                                Nothing -> (leftRow ++ rightRow, True)


    --Finds the indexes of the columns in both the left and right table to join on
    --Returns Nothing if there is no column join constraint
    getJoinColumns :: BooleanExpression -> Maybe (Int,Int)
    getJoinColumns (BooleanBracket expression) = getJoinColumns expression
    getJoinColumns (BooleanAND expression1 expression2)
      = case (getJoinColumns expression1, getJoinColumns expression2) of
          (Just _, Just _) -> error "You may only have column-join condition in a given join clause"
          (Just joinCols, Nothing) -> Just joinCols
          (Nothing, Just joinCols) -> Just joinCols
          (Nothing, Nothing) -> Nothing
    getJoinColumns (BooleanOR expression1 expression2)
      = case (getJoinColumns expression1, getJoinColumns expression2) of
          (Just _, Just _) -> error "You may only have column-join condition in a given join clause"
          (Just joinCols, Nothing) -> Just joinCols
          (Nothing, Just joinCols) -> Just joinCols
          (Nothing, Nothing) -> Nothing
    getJoinColumns (BooleanNOT expression)  = getJoinColumns expression
    getJoinColumns (BooleanEQ expression1 expression2)
      = case (getJoinColumns expression1, getJoinColumns expression2) of
          (Just _, Just _) -> error "You may only have column-join condition in a given join clause"
          (Just joinCols, Nothing) -> Just joinCols
          (Nothing, Just joinCols) -> Just joinCols
          (Nothing, Nothing) -> Nothing
    getJoinColumns (BooleanSubExpression se1 se2)
      = case (se1, se2) of
          (SubColumn leftColRef, SubColumn rightColRef) -> Just (getLabelIndex leftColRef tableEnv -1, getLabelIndex rightColRef tableEnv -1)
          (_,_) -> Nothing
    getJoinColumns (BooleanIndexExpression _ _)
      = Nothing



evalProduct :: TableName -> TableName -> TableEnvironment -> TableContent
evalProduct (TableRef name1) (TableRef name2) environment =
  [ map strip row1 ++ map strip row2 | row1 <- tableContent1, row2 <- tableContent2 ]
  where
    T tableContent1 _ = lookupTable (TableRef name1) environment
    T tableContent2 _ = lookupTable (TableRef name2) environment



evalSort :: TableName -> SortClause -> TableEnvironment -> TableContent
evalSort tableName sortClause tableEnvironment = sortedTable
  where
    table = lookupTable tableName tableEnvironment
    T tableContent _ = table
    sortedTable = case sortClause of
                            SortASC -> sortTable tableContent
                            SortDesc -> reverse (sortTable tableContent)

sortTable :: TableContent -> TableContent
sortTable table = sortedTable
  where
    strings = map unlines table
    sortedStrings = sort strings
    sortedTable = map lines sortedStrings




evalInsert :: String -> TableName -> Position -> TableEnvironment -> TableContent
evalInsert str tableName (Comma x y) environment
  | y > 0 && y <= length tableContent && x > 0 && x <= length (tableContent !! (y-1)) = updatedTable
  | otherwise = error "evalInsert: position out of bounds"
  where
    T tableContent _ = lookupTable tableName environment
    updatedRow = replaceAt (x-1) str (tableContent !! (y-1))
    updatedTable = replaceAt (y-1) updatedRow tableContent


evalFill :: String -> TableName -> Axis -> TableEnvironment -> TableContent
evalFill fillstr tableName axis environment = updated
  where
    T tableContent labels = lookupTable tableName environment
    arity = if null tableContent then 0 else length (head tableContent)
    columnIndex = case axis of
                ColumnInt n -> n
                ColumnAlpha label -> lookupLabel label labels
                Row _ -> error "evalFill: Cannot fill based on row axis"
    updated | columnIndex > 0 && columnIndex <= arity = map (replaceAt1 (columnIndex-1) fillstr "" ) tableContent
            | otherwise = error "evalFill: column index out of bounds"


replaceAt :: Int -> a -> [a] -> [a]
replaceAt i val xs = take i xs ++ [val] ++ drop (i + 1) xs

replaceAt1 :: Int -> a -> a -> [a] -> [a]
replaceAt1 i val def row
  | i < length row = take i row ++ [val] ++ drop (i+1) row
  | otherwise = row ++ replicate (i - length row) def ++ [val]

insertAt :: Int -> a -> [a] -> [a]
insertAt i val xs = take i xs ++ [val] ++ drop i xs

evalDelete :: TableName -> Axis -> TableEnvironment -> TableContent
evalDelete tableName axis environment =
  case axis of
    Row i
      | i > 0 && i <= length tableContent -> deleteRow (i-1) tableContent
      | otherwise -> error "evalDelete: row index out of bounds"
    ColumnInt i
      | i > 0 && i <= arity -> deleteColumn (i-1) tableContent
      | otherwise -> error "evalDelete: column index out of bounds"
    ColumnAlpha label ->
      case lookupLabel label labels of
        i | i > 0 && i <= arity -> deleteColumn (i-1) tableContent
          | otherwise -> error "evalDelete: column label index out of bounds"
  where
    T tableContent labels = lookupTable tableName environment
    arity = if null tableContent then 0 else length (head tableContent)

deleteRow :: Int -> [[a]] -> [[a]]
deleteRow i table = take i table ++ drop (i + 1) table

deleteColumn :: Int -> [[a]] -> [[a]]
deleteColumn i = map (removeAt i)

removeAt :: Int -> [a] -> [a]
removeAt i xs
  | i < length xs = take i xs ++ drop (i + 1) xs
  | otherwise = xs

evalClear :: TableName -> Position -> TableEnvironment -> TableContent
evalClear tableName (Comma x y) environment
  | y > 0 && y <= length tableContent && x > 0 && x <= length (tableContent !! (y-1)) = updatedTable
  | otherwise = error "evalClear: position out of bounds"
  where
    T tableContent _ = lookupTable tableName environment
    updatedRow = replaceAt (x-1) "" (tableContent !! (y-1))
    updatedTable = replaceAt (y-1) updatedRow tableContent


evalAddBlank :: TableName -> Axis -> TableEnvironment -> TableContent
evalAddBlank tableName axis environment = case axis of
  Row i
    | i <= (length tableContent + 1) && i > 0 -> insertAt (i-1) blankRow tableContent
    | otherwise -> error "evalAddBlank: row index out of bounds"
  ColumnInt i
    | i <= (arity+1) && i > 0 -> map (insertAt (i-1) "") tableContent
    | otherwise -> error "evalAddBlank: column index out of bounds"
  ColumnAlpha label ->
    case lookupLabel label labels of
      i | i <= (arity+1) && i > 0 -> map (insertAt (i-1) "") tableContent
        | otherwise -> error "evalAddBlank: column label index out of bounds"
  where
    T tableContent labels = lookupTable tableName environment
    arity = if null tableContent then 0 else length (head tableContent)
    blankRow = replicate arity ""



--Evaluates an output expression using a table environment, and outputs a specified table to a specified output channel
-- The output channels supported are standard output and outputting to a file with a specified name
evalOutput :: Output -> TableEnvironment -> IO()
evalOutput (OutputConstruct tableName outputType) tableEnvironment = do
                                                                  let table = lookupTable tableName tableEnvironment
                                                                  let stringTable = tableToString table
                                                                  case outputType of
                                                                    Standard -> putStr stringTable
                                                                    File filename -> writeFile filename stringTable

--Reformats a table into a string
tableToString :: Table -> String
tableToString (T tablecontent _) = finalTable
  where
    rowStrings = map (join ",") tablecontent
    finalTable = unlines rowStrings


--Finds a table in a given TableEnvironment using the name of the table
--Gives an error if the key doesn't exist in the environment
lookupTable :: TableName -> TableEnvironment -> Table
lookupTable (TableRef key) environment = read stringTable
  where
    lookupOutput = lookup key environment
    stringTable = case lookupOutput of
            Nothing -> error ("lookupTable: The table with the name'" ++ key ++ "' could not be found.")
            Just table -> show table

--Finds the index of a column using the label of the column, given a list of associations between labels and indexes
--Gives an error if the label doesn't exist in the list
lookupLabel :: String -> Labels -> Int
lookupLabel key environment = read stringLabel
  where
    lookupOutput = lookup key environment
    stringLabel = case lookupOutput of
            Nothing -> error ("lookupLabel: The table with the name'" ++ key ++ "' could not be found.")
            Just label -> show label

getLabelIndex :: ColumnReference -> TableEnvironment -> Int
getLabelIndex colRef tableEnvironment = index
  where
    (tableName, column, refIsLabel) = case  colRef of
                                  AlphaColumn name string -> (name, string, True)
                                  IntegerColumn name number -> (name, show number, False)
    T _ labels = lookupTable tableName tableEnvironment
    index | refIsLabel = lookupLabel column labels
          | otherwise = read column

--Returns all columns of a given table given a column and a TableEnvironment
getColumn :: ColumnReference -> TableEnvironment -> ColumnData
getColumn reference environment = dataInColumn
  where
    (tableName, column, refIsLabel) = case reference of
                                  AlphaColumn name string -> (name, string, True)
                                  IntegerColumn name number -> (name, show number, False)
    T tableContent labels = lookupTable tableName environment
    index | refIsLabel = lookupLabel column labels
          | otherwise = read column

    dataInColumn = [value | row <- tableContent,
                      let value = row!!(index-1)]