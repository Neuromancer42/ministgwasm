{
module Language.MiniStg.Lexer
    ( lexStg
    , StgToken(..)
    ) where

import Data.Text (Text)
import qualified Data.Text as T
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$upper = A-Z
$lower = a-z

tokens :-

  $white+               ;
  "--".*                ;
  \=                    { \_ -> T_Bind }
  \;                    { \_ -> T_Semicolon }
  "->"                  { \_ -> T_To }
  \\                    { \s -> T_Pi }
  let                   { \_ -> T_Let }
  letrec                { \_ -> T_Letrec }
  in                    { \_ -> T_In }
  case                  { \_ -> T_Case }
  of                    { \_ -> T_Of }
  $digit+"#"            { \s -> T_UnboxedInteger (read . init $ s) }
  "+#"                  { \_ -> T_Add }
  "-#"                  { \_ -> T_Sub }
  "*#"                  { \_ -> T_Mul }
  "/#"                  { \_ -> T_Div }
  "%#"                  { \_ -> T_Mod }
  "<#"                  { \_ -> T_Lt }
  "<=#"                 { \_ -> T_Leq }
  "==#"                 { \_ -> T_Eq }
  "/=#"                 { \_ -> T_Neq }
  ">=#"                 { \_ -> T_Gt }
  ">#"                  { \_ -> T_Gt }
  default               { \_ -> T_Default }
  [$lower \_][$alpha $digit \_ \']*     { \s -> T_VarId $ T.pack s }
  $upper[$alpha $digit \_]*\#?          { \s -> T_ConstrId $ T.pack s }

{

-- | The token type
data StgToken
    = T_Bind
    | T_Semicolon
    | T_To
    | T_Pi
    | T_Let
    | T_Letrec
    | T_In
    | T_Case
    | T_Of
    | T_UnboxedInteger Integer
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
    deriving (Show, Eq)

-- | The lexer of STG
lexStg :: String -> [StgToken]
lexStg = alexScanTokens
}
