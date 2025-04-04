{ 
module Lexer where 
}

%wrapper "posn" 
$digit = 0-9     
-- digits 
$alpha = [a-zA-Z]    
-- alphabetic characters

tokens :-
$white+         ; 
  "--".*        ; 
  INPUT                     { \p s -> PT p TokenINPUT} 
  OUTPUT                    { \p s -> PT p TokenOUTPUT }
  $digit+                   { \p s -> PT p (TokenInt (read s)) } 
  LET                       { \p s -> PT p TokenLET }
  BE                        { \p s -> PT p TokenBE }
  \{                        { \p s -> PT p TokenSquigleBracketL }
  \}                        { \p s -> PT p TokenSquigleBracketR }
  \,                        { \p s -> PT p TokenComma }
  \-                        { \p s -> PT p TokenDash }
  QUERIES END               { \p s -> PT p TokenQueriesEnd }
  PRODUCT                   { \p s -> PT p TokenPRODUCT}
  SORT                      { \p s -> PT p TokenSORT}
  INSERT                    { \p s -> PT p TokenINSERT}
  FILL                      { \p s -> PT p TokenFILL}
  DELETE                    { \p s -> PT p TokenDELETE}
  CLEAR                     { \p s -> PT p TokenCLEAR}
  COLUMN                    { \p s -> PT p TokenCOLUMN}
  ROW                       { \p s -> PT p TokenROW}
  $alpha [$alpha $digit \_ \']*   { \p s -> PT p (TokenVar s) } 

{ 

data PosnToken = PT AlexPosn Token deriving (Eq, Show)  
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenINPUT                | 
  TokenOUTPUT               | 
  TokenInt Int              |
  TokenVar String           | 
  TokenLET                  |
  TokenBE                   |
  TokenSquigleBracketL      |
  TokenSquigleBracketR      |
  TokenComma                |
  TokenDash                 |
  TokenQueriesEnd           |
  TokenPRODUCT              |
  TokenSORT                 |
  TokenINSERT               |
  TokenFILL                 |
  TokenDELETE               |
  TokenCLEAR                |
  TokenCOLUMN               |
  TokenROW                  
  deriving (Eq,Show) 

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn a b c) _) = show b ++":" ++ show c  

-- map tokenPosn (alexScanTokens "8*7") = ["1:1","1:2","1:3"]

}
