{
module Grammar where
import Lexer
}

%name parseCalc
%tokentype { PosnToken }
%error { parseError }

%token
  INPUT                 { PT _ TokenINPUT }
  OUTPUT                { PT _ TokenOUTPUT }
  LET                   { PT _ TokenLET }
  BE                    { PT _ TokenBE }
  QUERIESEND            { PT _ TokenQueriesEnd }
  MERGE                 { PT _ TokenMERGE }
  SELECT                { PT _ TokenSELECT }
  TO                    { PT _ TokenTO }
  STANDARD              { PT _ TokenSTANDARD }
  FILE                  { PT _ TokenFILE }
  PRODUCT               { PT _ TokenPRODUCT }
  SORT                  { PT _ TokenSORT }
  INSERT                { PT _ TokenINSERT }
  FILL                  { PT _ TokenFILL }
  DELETE                { PT _ TokenDELETE }
  CLEAR                 { PT _ TokenCLEAR }
  COLUMN                { PT _ TokenCOLUMN }
  COLUMNS               { PT _ TokenCOLUMNS } 
  ROW                   { PT _ TokenROW }
  FROMTABLES            { PT _ TokenFROMTABLES }
  FROM                  { PT _ TokenFROM}
  JOIN                  { PT _ TokenJOIN }
  INNER                 { PT _ TokenINNER }
  LEFT                  { PT _ TokenLEFT }
  RIGHT                 { PT _ TokenRIGHT }
  FULL                  { PT _ TokenFULL }
  OUTER                 { PT _ TokenOUTER }
  TABLE                 { PT _ TokenTABLE }
  AS                    { PT _ TokenAS }
  WITHLABELS            { PT _ TokenWITHLABELS }
  NOLABELS              { PT _ TokenNOLABELS }
  WHERE                 { PT _ TokenWHERE }
  WITHCONSTRAINT        { PT _ TokenWITHCONSTRAINT }
  AND                   { PT _ TokenAND }
  OR                    { PT _ TokenOR }
  NOT                   { PT _ TokenNOT }
  INDEX                 { PT _ TokenINDEX }
  MAX                   { PT _ TokenMAX }
  PLUS                  { PT _ TokenPLUSWORD}
  DISTINCT              { PT _ TokenDISTINCT}
  ASC                   { PT _ TokenASC }
  DESC                  { PT _ TokenDESC }
  ADDBLANKTO            { PT _ TokenADDBLANKTO }
  '='                   { PT _ TokenEQUAL }
  '+'                   { PT _ TokenPLUS }
  '*'                   { PT _ TokenMULTIPLY }
  '/'                   { PT _ TokenDIVSINGLE }
  "//"                  { PT _ TokenDIVTWO }
  '%'                   { PT _ TokenPERCENT }
  '^'                   { PT _ TokenEXP }
  '{'                   { PT _ TokenSquigleBracketL }
  '}'                   { PT _ TokenSquigleBracketR }
  '-'                   { PT _ TokenDash }
  ','                   { PT _ TokenComma }
  '['                   { PT _ TokenSquareBracketL }
  ']'                   { PT _ TokenSquareBracketR }
  '.'                   { PT _ TokenDot }
  '('                   { PT _ TokenBracketL }
  ')'                   { PT _ TokenBracketR }
  int                   { PT _ (TokenInt $$) }
  alphaLower            { PT _ (TokenAlphaLower $$) }
  alphaUpper            { PT _ (TokenAlphaUpper $$)}
  digits                { PT _ (TokenDigits $$)}
  Filename              { PT _ (TokenFilename $$)}
  alphaNumericString    { PT _ (TokenAlphaNumericString $$)}
  positiveNum           { PT _ (TokenPositiveNumber $$)}
  negativeNum           { PT _ (TokenNegativeNumber $$)}
  var                   { PT _ (TokenString $$)}

-- Arithmetic (high precedence)
%right '^'
%left '*' '/' "//" '%'
%left '+' '-'

-- Comparison
%nonassoc '='

-- Boolean (lower precedence)
%nonassoc NOT
%right AND
%right OR

-- Unary minus (lowest, disambiguated manually)
%nonassoc NEG

%%

Start : '{' Inputs '}' Queries Output                   { Start $2 $4 $5 }

Inputs : Input ',' Inputs                               { InputsCons $1 $3 }
       | Input                                          { InputSingle $1 }

Input : INPUT FILE Filename AS TableAssignment          { Input $3 $5 }

TableAssignment : TableName NOLABELS                    { NoLabels $1 }
        | TableName WITHLABELS ColumnLabels             { WithLabels $1 $3 }

TableName : alphaNumericString                          { TableRef $1 }

--Tables : TableName ',' TableName                        { TablesMultiple $1 $3 }
--        | TableName                                     { TableSingular $1 }
--

ColumnLabels : '[' ColumnLabels2 ']'                    { LabelConstructor $2 }

ColumnLabels2 : alphaNumericString ',' ColumnLabels2    { LabelsMultiple $1 $2}
                | alphaNumericString                    { LabelSingular $1} 

ColumnReference : TableName '.' alphaNumericString      { AlphaColumn $1 $3 }
        | TableName '.' int                             { IntegerColumn $1 $3 }

Output : OUTPUT TableName TO OutputType                 { OutputConstruct $2 $4 }

OutputType : STANDARD                                   { Standard }
           | FILE Filename                              { File $2 }
        
Position : int ',' int                                  { Comma $1 $3 }

Axis : COLUMN int                                       { ColumnInt $2 }
        | COLUMN alphaNumericString                     { ColumnAlpha $2 }
        | ROW int                                       { Row $2 }

Queries : LET TableAssignment BE Query '-' Queries      { QueryLet $2 $4 $6 }
        | QUERIESEND                                    { QueryEnd }

TableExpression : TableName                             { SingleTable $1 }
        | LET TableAssignment BE Query                  { TableLetQuery $2 $4 }
        | LET TableAssignment BE JoinClause             { TableLetJoin $2 $4 }
        | '(' TableExpression ')'                       { SingleTableExpression $2 }

Query : MERGE TableName TableName WITHCONSTRAINT BooleanExpression { Merge $2 $3 $5 }
      | SELECT Selection                                { Select $2}
      | PRODUCT TableName TableName                     { Product $2 $3 }
      | SORT TableName SortClause                       { Sort $2 $3 }
      | INSERT var TableName Position                   { Insert $2 $3 $4 }
      | FILL var TableName Axis                         { Fill $2 $3 $4 }
      | DELETE TableName Axis                           { Delete $2 $3 }
      | CLEAR TableName Position                        { Clear $2 $3 }
      | ADDBLANKTO TableName Axis                       { AddBank $2 $3}

Selection : SELECT MaxClause Distinct ColumnChoice FROM TableExpression WhereClause PlusClause {Selecter $2 $3 $4 $6 $7 $8}

SortClause : ASC                                        { SortASC }
        | DESC                                          { SortDesc }

MaxClause : MAX Number                                  { MaxTrue $2}
        |                                               { MaxFalse }

Distinct : DISTINCT                                     { DistinctTrue }
        |                                               { DistinctFalse }

ColumnChoice : COLUMNS '*'                              { ColumnALL }
        | COLUMN ColumnReference                        { ColumnSingle $2}
        | COLUMN ColumnReference ',' ColumnChoice       { ColumnMultiple $2 $4}

WhereClause : WHERE BooleanExpression                   { WhereTrue $2 }
        |                                               { WhereFalse }

PlusClause : PLUS Selection                             { PlusTrue $2}
        |                                               { PlusFalse }

JoinClause : TableExpression JoinOperator TableExpression WITHCONSTRAINT BooleanExpression { JoinClause $1 $2 $3 $5 }

JoinOperator : INNER JOIN                               { JoinInner }
        | LEFT JOIN                                     { JoinLeft }
        | RIGHT JOIN                                    { JoinRight }
        | FULL JOIN                                     { JoinFull }
        | OUTER JOIN                                    { JoinOuter }

BooleanExpression : '(' BooleanExpression ')'           { BooleanBracket $2 }
        | BooleanExpression AND BooleanExpression %prec AND { BooleanAND $1 $3 }
        | BooleanExpression OR BooleanExpression %prec OR { BooleanOR $1 $3 }
        | NOT BooleanExpression %prec NOT               { BooleanNOT $2 }
        | SubExpression '=' SubExpression  %prec '='    { BooleanSubExpression $1 $3 }

SubExpression : BooleanExpression                       { SubBoolean $1 }
        | ColumnReference                               { SubColumn $1 }
        | var                                           { SubString $1 }
        | IndexExpression %prec NEG                     { SubIndex $1 }

IndexExpression : INDEX                                 { IndexSingular }
        | Number                                        { IndexNum $1}
        | '(' IndexExpression ')'                       { IndexBracket $2 }
        | IndexExpression '+' IndexExpression           { IndexPlus $1 $3 }
        | IndexExpression '-' IndexExpression           { IndexMinus $1 $3 } 
        | IndexExpression '*' IndexExpression           { IndexMult $1 $3 } 
        | IndexExpression '/' IndexExpression           { IndexSingularDiv $1 $3 } 
        | IndexExpression "//" IndexExpression          { IndexTwoDiv $1 $3 } 
        | IndexExpression '%' IndexExpression           { IndexPercent $1 $3 } 
        | IndexExpression '^' IndexExpression           { IndexExpo $1 $3 }

Number : positiveNum                                    { PositiveNumber }
        | negativeNum                                   { NegativeNumber }

{
parseError :: [PosnToken] -> a
parseError _ = error "Parse error"

data Start = Start Inputs Queries Output deriving Show

data Inputs = InputsCons Input Inputs
                | InputSingle Input
                deriving Show

data Input = Input String TableAssignment deriving Show

data TableAssignment = NoLabels TableName
                | WithLabels TableName ColumnLabels
                deriving Show

data TableName = TableRef String deriving Show

--data Tables = TablesMultiple TableName Tables 
--                | TableSingular TableName
--                deriving Show 

data ColumnLabels = LabelConstructor ColumnLabels2 deriving Show

data ColumnLabels2 = LabelsMultiple String ColumnLabels2 
                | LabelSingular String
                deriving Show

data ColumnReference = AlphaColumn TableName String
                | IntegerColumn TableName Int
                deriving Show

data Output = OutputConstruct TableName OutputType deriving Show

data OutputType = Standard
                | File String
                deriving Show

data Position = Comma Int Int deriving Show

data Queries = QueryLet TableAssignment Query Queries
                | QueryEnd
                deriving Show

data TableExpression = SingleTable TableName
                | TableLetQuery TableAssignment Query
                | TableLetJoin TableAssignment JoinClause
                | SingleTableExpression TableExpression
                deriving Show

data Query = Merge TableName TableName BooleanExpression
                | Select Selection
                | Product TableName TableName
                | Sort TableName SortClause
                | Insert String TableName Position
                | Fill String TableName Axis
                | Delete TableName Axis
                | Clear TableName Position
                | AddBank TableName Axis
                deriving Show


data Axis = ColumnInt Int
                | ColumnAlpha String
                | Row Int
                deriving Show


data Selection = Selecter MaxClause Distinct ColumnChoice TableExpression WhereClause PlusClause deriving Show

data SortClause = SortASC | SortDesc deriving Show

data MaxClause = MaxTrue Number
                | MaxFalse
                deriving Show

data Distinct = DistinctTrue
                | DistinctFalse
                deriving Show

data ColumnChoice = ColumnALL 
                | ColumnSingle ColumnReference
                | ColumnMultiple ColumnReference ColumnChoice
                deriving Show

data WhereClause = WhereTrue BooleanExpression
                | WhereFalse 
                deriving Show

data PlusClause = PlusTrue Selection
                | PlusFalse
                deriving Show

data JoinClause = JoinClause TableExpression JoinOperator TableExpression BooleanExpression deriving Show

data JoinOperator = JoinInner
                | JoinLeft
                | JoinRight
                | JoinFull
                | JoinOuter
                deriving Show

data BooleanExpression = BooleanBracket BooleanExpression
                | BooleanAND BooleanExpression BooleanExpression
                | BooleanOR BooleanExpression BooleanExpression
                | BooleanNOT BooleanExpression
                | BooleanSubExpression SubExpression SubExpression
                deriving Show

data SubExpression = SubBoolean BooleanExpression
                | SubColumn ColumnReference
                | SubString String
                | SubIndex IndexExpression
                deriving Show

data IndexExpression = IndexSingular
                | IndexNum Number
                | IndexBracket IndexExpression
                | IndexPlus IndexExpression IndexExpression
                | IndexMinus IndexExpression IndexExpression
                | IndexMult IndexExpression IndexExpression
                | IndexSingularDiv IndexExpression IndexExpression
                | IndexTwoDiv IndexExpression IndexExpression
                | IndexPercent IndexExpression IndexExpression
                | IndexExpo IndexExpression IndexExpression
                deriving Show


data Number = PositiveNumber 
                | NegativeNumber 
                deriving Show

}
