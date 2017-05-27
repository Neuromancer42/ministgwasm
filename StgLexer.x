{
module StgLexer
    ( lexStg
    , StgToken(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$upper = [A-Z]
$lower = [a-z]

tokens :-

  $white+               ;
  "--".*                ;
  "="                   { \s -> T_Bind }
  ";"                   { \s -> T_Semicolon }
  "->"                  { \s -> T_To }
  "\\"$lower            { \s -> T_Pi (head . tail $ s) }
  let                   { \s -> T_Let }
  letrec                { \s -> T_Letrec }
  in                    { \s -> T_In }
  case                  { \s -> T_Case }
  of                    { \s -> T_Of }
  $digit+"#"            { \s -> T_UnboxedInt (read . init $ s) }
  "+#"                  { \s -> T_Add }
  "-#"                  { \s -> T_Sub }
  "*#"                  { \s -> T_Mul }
  "/#"                  { \s -> T_Div }
  "%#"                  { \s -> T_Mod }
  "<#"                  { \s -> T_Lt }
  "<=#"                 { \s -> T_Leq }
  "==#"                 { \s -> T_Eq }
  "/=#"                 { \s -> T_Neq }
  ">=#"                 { \s -> T_Gt }
  ">#"                  { \s -> T_Gt }
  default               { \s -> T_Default }
  [$lower \_][$alpha $digit \_ \']      { \s -> T_VarId $ T.pack s }
  $upper[$alpha $digit \_]"#"           { \s -> T_ConstrId $ T.pack s }

{

-- | The token type
data StgToken
    = T_Bind
    | T_Semicolon
    | T_To
    | T_Pi Char
    | T_Let
    | T_Letrec
    | T_In
    | T_Case
    | T_Of
    | T_UnboxedInt Int
    | T_Add
    | T_Sub
    | T_Mul
    | T_Div
    | T_Mod
    | T_Lt
    | T_Leq
    | T_Eq
    | T_Neq
    | T_Geq
    | T_Gt
    | T_Default
    | T_VarId Text
    | T_ConstrId Text

-- | The lexer of STG
lexStg :: String -> [StgToken]
lexStg = alexScanTokens
}
