{
module Grammar where
import Lexer
}

%name parseCalc
%tokentype { PosnToken }
%error { parseError }

%token
  INPUT           { PT _ TokenINPUT }
  OUTPUT          { PT _ TokenOUTPUT }
  LET             { PT _ TokenLET }
  BE              { PT _ TokenBE }
  QUERIESEND      { PT _ TokenQueriesEnd }
  MERGE           { PT _ TokenMERGE }
  SELECT          { PT _ TokenSELECT }
  TO              { PT _ TokenTO }
  STANDARD        { PT _ TokenSTANDARD }
  FILE            { PT _ TokenFILE }
  PRODUCT         { PT _ TokenPRODUCT }
  SORT            { PT _ TokenSORT }
  INSERT          { PT _ TokenINSERT }
  FILL            { PT _ TokenFILL }
  DELETE          { PT _ TokenDELETE }
  CLEAR           { PT _ TokenCLEAR }
  COLUMN          { PT _ TokenCOLUMN }
  ROW             { PT _ TokenROW }
  FROMTABLES      { PT _ TokenFROMTABLES }
  JOIN            { PT _ TokenJOIN }
  TABLE           { PT _ TokenTABLE }
  int             { PT _ (TokenInt $$) }
  var             { PT _ (TokenVar $$) }
  '{'             { PT _ TokenSquigleBracketL }
  '}'             { PT _ TokenSquigleBracketR }
  '-'             { PT _ TokenDash }
  ','             { PT _ TokenComma }

%%

Start : '{' Inputs '}' Queries Output       { Start $2 $4 $5 }

Inputs : Input ',' Inputs                   { InputsCons $1 $3 }
       |                                    { InputsNil }

Input : INPUT var                           { Input $2 }

Queries : LET Table BE Query '-' Queries    { QueryLet $2 $4 $6 }
        | Query '-' Queries                 { QueryDash $1 $3 }
        | QUERIESEND                        { QueryEnd }


Query : MERGE                               { Merge }
      | SELECT Selection                    { Select $2 }
      | PRODUCT                             { Product }
      | SORT                                { Sort }
      | INSERT var Table Position           { Insert $2 $3 $4 }
      | FILL var Table Axis                 { Fill $2 $3 $4 }
      | DELETE Table Axis                   { Delete $2 $3 }
      | CLEAR Table Position                { Clear $2 $3 }

Selection : FROMTABLES Table                { FromSelection $2 }
          | TABLE Table                     { TableOnly $2 }
          | TABLE Table Position            { TableWithPosition $2 $3 }
          | TABLE Table Axis                { TableWithAxis $2 $3 }

Position : int ',' int                      { Comma $1 $3 }

Axis : COLUMN int                           { Column $2 }
     | ROW int                              { Row $2 }

Table : var                                 { TableRef $1 }

Output : OUTPUT Table TO OutputType         { OutputConstruct $2 $4 }

OutputType : STANDARD                       { Standard }
           | FILE var                       { File $2 }



{
parseError :: [PosnToken] -> a
parseError _ = error "Parse error"

data Start = Start Inputs Queries Output deriving Show

data Inputs = InputsCons Input Inputs
            | InputsNil
            deriving Show

data Input = Input String deriving Show

data Table = TableRef String deriving Show

data Position = Comma Int Int deriving Show

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

data Axis = Column Int
          | Row Int
          deriving Show

data Output = OutputConstruct Table OutputType deriving Show

data OutputType = Standard
                | File String
                deriving Show
}
