{
module Grammar where
import Lexer
}

%name parseCalc
%tokentype { Token }
%error { parseError }

%token
  INPUT           { TokenINPUT }
  OUTPUT          { TokenOUTPUT }
  LET             { TokenLET }
  BE              { TokenBE }
  QUERIESEND     { TokenQueriesEnd }
  MERGE           { TokenMERGE }
  SELECT          { TokenSELECT }
  TO              { TokenTO }
  STANDARD        { TokenSTANDARD }
  FILE            { TokenFILE }
  PRODUCT         { TokenPRODUCT }
  SORT            { TokenSORT }
  INSERT          { TokenINSERT }
  FILL            { TokenFILL }
  DELETE          { TokenDELETE }
  CLEAR           { TokenCLEAR }
  COLUMN          { TokenCOLUMN }
  ROW             { TokenROW }
  FROMTABLES     { TokenFROMTABLES }
  JOIN            { TokenJOIN }
  TABLE           { TokenTABLE }
  int             { TokenInt $$ }
  var             { TokenVar $$ }
  '{'             { TokenSquigleBracketL }
  '}'             { TokenSquigleBracketR }
  '-'             { TokenDash }
  ','             { TokenComma }

%%

Start : '{' INPUT Inputs '}' Queries Output { Start $3 $5 $6 }

Inputs : Input Inputs { InputsCons $1 $2 }
       |               { InputsNil }

Input : INPUT var { Input $2 }

Queries : LET Table BE Query Queries { QueryLet $2 $4 $5 }
        | Query '-' Queries         { QueryDash $1 $3 }
        | QUERIESEND               { QueryEnd }


Query : MERGE                      { Merge }
      | SELECT Selection           { Select $2 }
      | PRODUCT                    { Product }
      | SORT                       { Sort }
      | INSERT var Table Position { Insert $2 $3 $4 }
      | FILL var Table Axis       { Fill $2 $3 $4 }
      | DELETE Table Axis         { Delete $2 $3 }
      | CLEAR Table Position      { Clear $2 $3 }

Selection : FROMTABLES Table      { FromSelection $2 }
          | TABLE Table            { TableOnly $2 }
          | TABLE Table Position   { TableWithPosition $2 $3 }
          | TABLE Table Axis       { TableWithAxis $2 $3 }

Position : int ',' int { COMMA $1 $3 }

Axis : COLUMN int { COLUMN $2 }
     | ROW int    { ROW $2 }

Table : var { TableRef $1 }

Output : OUTPUT Table TO OutputType { OutputConstruct $2 $4 }

OutputType : STANDARD    { Standard }
           | FILE var    { File $2 }

%%

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Start = Start Inputs Queries Output deriving Show

data Inputs = InputsCons Input Inputs
            | InputsNil
            deriving Show

data Input = Input String deriving Show

data Table = TableRef String deriving Show

data Position = COMMA Int Int deriving Show

data Queries = QueryLet Table Query Queries
             | QueryDash Query Queries
             | QueryEnd
             deriving Show


data Query = Merge
           | Select Selection
           | Product
           | Sort
           | Insert String Table Position
           | Fill String Table Axis
           | Delete Table Axis
           | Clear Table Position
           deriving Show

data Selection = FromSelection Table
               | TableOnly Table
               | TableWithPosition Table Position
               | TableWithAxis Table Axis
               deriving Show

data Axis = COLUMN Int
          | ROW Int
          deriving Show

data Output = OutputConstruct Table OutputType deriving Show

data OutputType = Standard
                | File String
                deriving Show
}
