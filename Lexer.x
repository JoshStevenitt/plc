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
  INPUT         { \p s -> PT p TokenINPUT} 
  OUTPUT        { \p s -> PT p TokenOUTPUT }
  $digit+       { \p s -> PT p (TokenInt (read s)) } 
  \=            { \p s -> PT p TokenEq }
  \+            { \p s -> PT p TokenPlus }
  \-            { \p s -> PT p TokenMinus }
  \*            { \p s -> PT p TokenTimes }
  \/            { \p s -> PT p TokenDiv }
  \(            { \p s -> PT p TokenLParen }
  \)            { \p s -> PT p TokenRParen }
  \^            { \p s -> PT p TokenExpenential}
  $alpha [$alpha $digit \_ \']*   { \p s -> PT p (TokenVar s) } 

{ 

data PosnToken = PT AlexPosn Token deriving (Eq, Show)  
-- Each action has type :: String -> Token 
-- The token type: 
data Token = 
  TokenINPUT       | 
  TokenOUTPUT      | 
  TokenInt Int     |
  TokenVar String  | 
  TokenEq          |
  TokenPlus        |
  TokenMinus       |
  TokenTimes       |
  TokenDiv         |
  TokenLParen      |
  TokenRParen      |
  TokenExpenential 
  deriving (Eq,Show) 

tokenPosn :: PosnToken -> String
tokenPosn (PT (AlexPn a b c) _) = show b ++":" ++ show c  

-- map tokenPosn (alexScanTokens "8*7") = ["1:1","1:2","1:3"]

}
