{
module Language.MiniStg.Parser
  ( parseStg
  ) where

import Language.MiniStg.Lexer (lexStg, StgToken (..))
import Language.MiniStg

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

}

%name parseStgToken prog
%tokentype { StgToken }
%error { parseError }

%token
  '='       { T_Bind }
  ';'       { T_Semicolon }
  to        { T_To }
  pi        { T_Pi }
  let       { T_Let }
  letrec    { T_Letrec }
  in        { T_In }
  case      { T_Case }
  of        { T_Of }
  integer   { T_UnboxedInteger $$ }
  add       { T_Add }
  sub       { T_Sub }
  mul       { T_Mul }
  div       { T_Div }
  mod       { T_Mod }
  lt        { T_Lt }
  leq       { T_Leq }
  eq        { T_Eq }
  neq       { T_Neq }
  geq       { T_Geq }
  gt        { T_Gt }
  varid     { T_VarId $$ }
  constrid  { T_ConstrId $$ }
  default   { T_Default }

%%

prog :: { Program }
     : binds                            { Program (Binds $1) }

binds :: { Map Var LambdaForm }
      : binds ';' var '=' lf            { M.insert $3 $5 $1 }
      | var '=' lf                      { M.singleton $1 $3 }

lf :: { LambdaForm }
   : vars pi vars to expr      { LambdaForm $1 Updatable $3 $5 }
   | pi vars to expr           { LambdaForm [] Updatable $2 $4 }
   | vars pi to expr           { LambdaForm $1 Updatable [] $4 }
   | pi to expr                { LambdaForm [] Updatable [] $3 }

expr :: { Expr }
     : let binds in expr                { Let NonRecursive (Binds $2) $4 }
     | letrec binds in expr             { Let Recursive (Binds $2) $4 }
     | case expr of alts                { Case $2 $4 }
     | var atoms                        { AppF $1 $2 }
     | var                              { AppF $1 [] }
     | constr atoms                     { AppC $1 $2 }
     | constr                           { AppC $1 [] }
     | prim atom atom                   { AppP $1 $2 $3 }
     | literal                          { LitE $1 }

alts :: { Alts }
     : nondefaultalts defaultalt        { Alts $1 $2 }

nondefaultalts :: { NonDefaultAlts }
               : {- empty -}            { NoNonDefaultAlts }
               | algebraicalts          { AlgebraicAlts (NE.reverse $1) }
               | primitivealts          { PrimitiveAlts (NE.reverse $1) }

algebraicalts :: { NonEmpty AlgebraicAlt }
              : algebraicalts algebraicalt ';'
                        { NE.cons $2 $1 }
              | algebraicalt ';'
                        { $1 :| [] }

algebraicalt :: { AlgebraicAlt }
             : constr vars to expr { AlgebraicAlt $1 $2 $4 }

primitivealts :: { NonEmpty PrimitiveAlt }
              : primitivealts primitivealt ';'
                        { NE.cons $2 $1 }
              | primitivealt ';'
                        { $1 :| [] }

primitivealt :: { PrimitiveAlt }
             : literal to expr          { PrimitiveAlt $1 $3 }

defaultalt :: { DefaultAlt }
           : default to expr            { DefaultNotBound $3 }
           | var to expr                { DefaultBound $1 $3 }

literal :: { Literal }
        : integer                        { Literal $1 }

prim :: { PrimOp }
     : add          { Add }
     | sub          { Sub }
     | mul          { Mul }
     | div          { Div }
     | mod          { Mod }
     | lt           { Lt  }
     | leq          { Leq }
     | eq           { Eq  }
     | neq          { Neq }
     | geq          { Geq }
     | gt           { Gt  }

var :: { Var }
    : varid         { Var $1 }

vars :: { [Var] }
     : vars var     { $1 ++ [$2] }
     | var          { [$1] }

atom :: { Atom }
     : var          { AtomVar $1 }
     | literal      { AtomLit $1 }

atoms :: { [Atom] }
      : atoms atom  { $1 ++ [$2] }
      | atom        { [$1] }

constr :: { Constr }
       : constrid   { Constr $1 }

{

parseError :: [StgToken] -> a
parseError _ = error "Parse error"

-- | The overall STG parser
parseStg :: String -> Program
parseStg = parseStgToken . lexStg

}
