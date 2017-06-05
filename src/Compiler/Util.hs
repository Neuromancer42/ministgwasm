module Compiler.Util where

import Data.Char
import LLVM.AST
import LLVM.AST.AddrSpace
import qualified Language.MiniStg as STG

int :: Type
int = IntegerType 64

intptr :: Type
intptr = PointerType int (AddrSpace 0)

intintfunc :: Type
intintfunc = FunctionType int [int] False

intintfptr :: Type
intintfptr = PointerType intintfunc (AddrSpace 0)

-- | referer tag, refer to an evaled value
-- either a direct literal or an indirect construction
evalLitTag :: Integer
evalLitTag = 0

-- | referer tag, refer to an evaled construction (directly)
evalConTag :: Integer
evalConTag = 2

-- | referer tag, refer to an unevaled primitive expression
unevalPrimTag :: Integer
unevalPrimTag = 1

-- | referer tag, refer to an unevaled function application
unevalFuncTag :: Integer
unevalFuncTag = 3

-- | referee tag, it's unvaluated, either
-- a function application or a primitive operation.
funcTag :: Integer
funcTag = 1

-- | referee tag, it's an evaluated literal
litTag :: Integer
litTag = 0

-- | referee tag, it's an indirect construction pointer
indTag :: Integer
indTag = 2

-- | encode characters into numbers
encChar :: Char -> Integer
encChar c
  | '0' <= c && c <= '9' = fromIntegral $ ord c - ord '0'
encChar c
  | 'A' <= c && c <= 'Z' = fromIntegral $ ord c - ord 'A' + 10
encChar c
  | 'a' <= c && c <= 'z' = fromIntegral $ ord c - ord 'a' + 36
encChar '_' = 62
encChar '#' = 63
encChar _ = error "Syntax Error: Invalid character"

-- | encode operations into numbers
encOp :: STG.PrimOp -> Integer
encOp p =
  case p of
    STG.Add -> 0
    STG.Sub -> 1
    STG.Mul -> 2
    STG.Div -> 3
    STG.Mod -> 4
    _ -> error "Error: primitive function not supported"

-- | decode operations from numbers
decOp :: Integer -> STG.PrimOp
decOp n =
  case n of
    0 -> STG.Add
    1 -> STG.Sub
    2 -> STG.Mul
    3 -> STG.Div
    4 -> STG.Mod
    _ -> error "Error: primitive function not supported"

-- | encode a constructor into a number
encConstr :: String -> Integer -> Integer
encConstr s l = foldr (\ch i -> encChar ch + i * 64) 0 s * 8 + (fromIntegral l * 2)
