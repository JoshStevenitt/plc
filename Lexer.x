{
module Lexer where
}

%wrapper "posn"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaLower = a-z
$alphaUpper = A-Z
$char = [^\'\n\\]


tokens :-
  $white+                         ;
  "--".*                          ;

  INPUT                           { \p s -> PT p TokenINPUT }
  OUTPUT                          { \p s -> PT p TokenOUTPUT }
  LET                             { \p s -> PT p TokenLET }
  BE                              { \p s -> PT p TokenBE }
  QUERIESEND                      { \p s -> PT p TokenQueriesEnd }
  MERGE                           { \p s -> PT p TokenMERGE }
  SELECT                          { \p s -> PT p TokenSELECT }
  TO                              { \p s -> PT p TokenTO }
  STANDARD                        { \p s -> PT p TokenSTANDARD }
  FILE                            { \p s -> PT p TokenFILE }
  PRODUCT                         { \p s -> PT p TokenPRODUCT }
  SORT                            { \p s -> PT p TokenSORT }
  INSERT                          { \p s -> PT p TokenINSERT }
  FILL                            { \p s -> PT p TokenFILL }
  DELETE                          { \p s -> PT p TokenDELETE }
  CLEAR                           { \p s -> PT p TokenCLEAR }
  COLUMN                          { \p s -> PT p TokenCOLUMN }
  COLUMNS                         { \p s -> PT p TokenCOLUMNS }
  ROW                             { \p s -> PT p TokenROW }
  FROMTABLES                      { \p s -> PT p TokenFROMTABLES }
  FROM                            { \p s -> PT p TokenFROM }
  JOIN                            { \p s -> PT p TokenJOIN }
  INNER                           { \p s -> PT p TokenINNER }
  LEFT                            { \p s -> PT p TokenLEFT }
  RIGHT                           { \p s -> PT p TokenRIGHT }
  FULL                            { \p s -> PT p TokenFULL }
  OUTER                           { \p s -> PT p TokenOUTER }
  TABLE                           { \p s -> PT p TokenTABLE }
  AS                              { \p s -> PT p TokenAS }
  WITHLABELS                      { \p s -> PT p TokenWITHLABELS }
  NOLABELS                        { \p s -> PT p TokenNOLABELS }
  WHERE                           { \p s -> PT p TokenWHERE }
  WITHCONSTRAINT                  { \p s -> PT p TokenWITHCONSTRAINT }
  ASC                             { \p s -> PT p TokenASC }
  DESC                            { \p s -> PT p TokenDESC }
  ADDBLANKTO                      { \p s -> PT p TokenADDBLANKTO }
  AND                             { \p s -> PT p TokenAND }
  OR                              { \p s -> PT p TokenOR }
  NOT                             { \p s -> PT p TokenNOT }
  MAX                             { \p s -> PT p TokenMAX}
  DISTINCT                        { \p s -> PT p TokenDISTINCT}
  PLUS                            { \p s -> PT p TokenPLUSWORD}
  \=                              { \p s -> PT p TokenEQUAL }
  INDEX                           { \p s -> PT p TokenINDEX }
  \+                              { \p s -> PT p TokenPLUS }
  \*                              { \p s -> PT p TokenMULTIPLY }
  \/                              { \p s -> PT p TokenDIVSINGLE }
  "//"                            { \p s -> PT p TokenDIVTWO }
  \%                              { \p s -> PT p TokenPERCENT }
  \^                              { \p s -> PT p TokenEXP }
  \{                              { \p s -> PT p TokenSquigleBracketL }
  \}                              { \p s -> PT p TokenSquigleBracketR }
  \,                              { \p s -> PT p TokenComma }
  \-                              { \p s -> PT p TokenDash }
  \[                              { \p s -> PT p TokenSquareBracketL }
  \]                              { \p s -> PT p TokenSquareBracketR }
  \.                              { \p s -> PT p TokenDot }
  \(                              { \p s -> PT p TokenBracketL }
  \)                              { \p s -> PT p TokenBracketR }
  $digit                          { \p s -> PT p (TokenInt (read s)) }
  $alphaLower                     { \p s -> PT p (TokenAlphaLower s) }
  $alphaUpper                     { \p s -> PT p (TokenAlphaUpper s) }
  \"[$alpha]+\.[$alpha]+\"        { \p s -> PT p (TokenFilename (init (tail s))) }
  [$alpha$digit]+                 { \p s -> PT p (TokenAlphaNumericString s) }
  [$digit]+                       { \p s -> PT p (TokenDigits s) }
  [\+]? $digit+ (\. $digit+)?     { \p s -> PT p (TokenPositiveNumber s) }
  [\-]? $digit+ (\. $digit+)?     { \p s -> PT p (TokenNegativeNumber s) }
  \'[$char]*\'                    { \p s -> PT p (TokenString s) }
{
data PosnToken = PT AlexPosn Token deriving (Eq, Show)

data Token =
    TokenINPUT
  | TokenOUTPUT
  | TokenInt Int
  | TokenAlphaLower String
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
  | TokenCOLUMNS
  | TokenROW
  | TokenFROMTABLES
  | TokenFROM
  | TokenJOIN
  | TokenINNER
  | TokenLEFT
  | TokenRIGHT
  | TokenFULL
  | TokenOUTER
  | TokenTABLE
  | TokenAS 
  | TokenWITHLABELS
  | TokenNOLABELS
  | TokenWHERE
  | TokenWITHCONSTRAINT
  | TokenAND
  | TokenOR
  | TokenNOT
  | TokenEQUAL
  | TokenINDEX
  | TokenPLUS
  | TokenMULTIPLY
  | TokenDIVSINGLE
  | TokenDIVTWO
  | TokenPERCENT
  | TokenEXP
  | TokenSquareBracketL
  | TokenSquareBracketR
  | TokenDot
  | TokenBracketL
  | TokenBracketR
  | TokenMAX
  | TokenDISTINCT
  | TokenPLUSWORD
  | TokenADDBLANKTO
  | TokenASC
  | TokenDESC
  | TokenFilename String
  | TokenAlphaNumericString String
  | TokenAlphaUpper String
  | TokenDigits Int
  | TokenPositiveNumber String
  | TokenNegativeNumber String
  | TokenString String
  deriving (Eq, Show)

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn _ line col) _) = show line ++ ":" ++ show col
}
