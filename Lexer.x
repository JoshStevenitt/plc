{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]

tokens :-
  $white+             ;
  "--".*              ;

  INPUT               { \p s -> PT p TokenINPUT }
  OUTPUT              { \p s -> PT p TokenOUTPUT }
  LET                 { \p s -> PT p TokenLET }
  BE                  { \p s -> PT p TokenBE }
  QUERIESEND          { \p s -> PT p TokenQueriesEnd }
  MERGE               { \p s -> PT p TokenMERGE }
  SELECT              { \p s -> PT p TokenSELECT }
  TO                  { \p s -> PT p TokenTO }
  STANDARD            { \p s -> PT p TokenSTANDARD }
  FILE                { \p s -> PT p TokenFILE }
  PRODUCT             { \p s -> PT p TokenPRODUCT }
  SORT                { \p s -> PT p TokenSORT }
  INSERT              { \p s -> PT p TokenINSERT }
  FILL                { \p s -> PT p TokenFILL }
  DELETE              { \p s -> PT p TokenDELETE }
  CLEAR               { \p s -> PT p TokenCLEAR }
  COLUMN              { \p s -> PT p TokenCOLUMN }
  ROW                 { \p s -> PT p TokenROW }
  FROMTABLES          { \p s -> PT p TokenFROMTABLES }
  JOIN                { \p s -> PT p TokenJOIN }
  TABLE               { \p s -> PT p TokenTABLE }
  AS                  { \p s -> PT p TokenAS }
  WITHLABELS          { \p s -> PT p TokenWITHLABELS }
  NOLABELS            { \p s -> PT p TokenNOLABELS }
  \{                  { \p s -> PT p TokenSquigleBracketL }
  \}                  { \p s -> PT p TokenSquigleBracketR }
  \,                  { \p s -> PT p TokenComma }
  \-                  { \p s -> PT p TokenDash }
  \[                  { \p s -> PT p TokenSquareBracketL }
  \]                  { \p s -> PT p TokenSquareBracketR }
  $digit+             { \p s -> PT p (TokenInt (read s)) }
  $alpha [$alpha $digit \_ \']* { \p s -> PT p (TokenVar s) }

{
data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token =
    TokenINPUT
  | TokenOUTPUT
  | TokenInt Int
  | TokenVar String
  | TokenLET
  | TokenBE
  | TokenSquigleBracketL
  | TokenSquigleBracketR
  | TokenComma
  | TokenDash
  | TokenQueriesEnd
  | TokenMERGE
  | TokenSELECT
  | TokenTO
  | TokenSTANDARD
  | TokenFILE
  | TokenPRODUCT
  | TokenSORT
  | TokenINSERT
  | TokenFILL
  | TokenDELETE
  | TokenCLEAR
  | TokenCOLUMN
  | TokenROW
  | TokenFROMTABLES
  | TokenJOIN
  | TokenTABLE
  | TokenAS 
  | TokenWITHLABELS
  | TokenNOLABELS
  | TokenSquareBracketL
  | TokenSquareBracketR
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}
