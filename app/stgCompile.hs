module Main where

import Compiler.Translate
import Control.Monad.Except
import LLVM.AST
import qualified LLVM.AST as AST
import LLVM.Context
import LLVM.Module
import Language.MiniStg as STG
import Language.MiniStg.Parser (parseStg)
import System.Environment
import System.Exit
import System.IO

main :: IO ()
main = do
  args <- getArgs
  case args of
    [ifile, ofile] -> do
      input <- readFile ifile
      let ast = parseStg input
      let defs = trProgram ast
      let m = defaultModule {moduleName = "test", moduleDefinitions = defs}
      toLLVM m

toLLVM :: AST.Module -> IO ()
toLLVM m =
  withContext $ \ctx -> do
    errOrLLVM <- runExceptT $ withModuleFromAST ctx m moduleLLVMAssembly
    case errOrLLVM of
      Left err -> putStrLn $ "error: " ++ err
      Right llvm -> putStrLn llvm
