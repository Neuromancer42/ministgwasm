module Compiler.Translate where

import Compiler.Util
import qualified Data.List as L
import qualified Data.Map.Lazy as M
import qualified Data.Set as S
import qualified Data.Text as T
import LLVM.AST
import LLVM.AST.CallingConvention
import qualified LLVM.AST.Constant as Const
import qualified LLVM.AST.Global as Gbl
import qualified Language.MiniStg as STG

-- | translate localbindings to mallocs.
-- (treat all let-binds as recursive)
-- allocate space for binders first
-- and then fill in all the arguments.
trLocalBinds :: S.Set T.Text -> M.Map STG.Var STG.LambdaForm -> (S.Set T.Text, [Named Instruction])
trLocalBinds outerRef binds =
  (localRef, concatMap (uncurry trPreBind) kvps ++ concatMap (uncurry (trFillBind localRef)) kvps)
  where
    kvps = M.assocs binds
    localRef = S.map (\(STG.Var v) -> v) (M.keysSet binds) `S.union` outerRef

-- | fill arguments in thunks
trFillBind :: S.Set T.Text -> STG.Var -> STG.LambdaForm -> [Named Instruction]
trFillBind localRef (STG.Var bndr) (STG.LambdaForm [] _ [] expr) =
  let bndrname = T.unpack bndr
      allocname = Name $ bndrname ++ "_al"
  in case expr of
       STG.Let {} -> error "Syntax Error: Let expression in local binding not lifted"
       STG.Case _ _ -> error "Syntax Error: Case expression in local binding not lifted"
              -- ^ complex expressions are not allowed in local bindings
       STG.AppF (STG.Var f) as ->
         let addrname = bndrname ++ "_ad0"
         in (Name addrname :=
             IntToPtr {operand0 = LocalReference int allocname, type' = intptr, metadata = []}) :
            Do
              Store
              { volatile = False
              , address = LocalReference intptr $ Name addrname
              , value = ConstantOperand $ trFunc f (L.length as)
              , maybeAtomicity = Nothing
              , alignment = 8
              , metadata = []
              } :
            concat (zipWith (trArg bndrname localRef) as [1 ..])
       STG.AppC (STG.Constr c) as ->
         let addrname = bndrname ++ "_ad0"
         in (Name addrname :=
             IntToPtr {operand0 = LocalReference int allocname, type' = intptr, metadata = []}) :
            Do
              Store
              { volatile = False
              , address = LocalReference intptr $ Name addrname
              , value = ConstantOperand $ trConstr c (L.length as)
              , maybeAtomicity = Nothing
              , alignment = 8
              , metadata = []
              } :
            concat (zipWith (trArg bndrname localRef) as [1 ..])
       STG.AppP {} -> error "Syntax error: primitive operation in local binding not supported"
       STG.LitE _ -> []
  where
    trFunc :: T.Text -> Int -> Const.Constant
    trFunc f ll =
      let l = fromIntegral ll
      in if l > 3
           then error "Syntax Error: Arguments more than 3 not supported"
           else let fname = Name $ T.unpack f
                in Const.Add False False (Const.Int 64 (l * 2 + funcTag)) $
                   Const.Mul
                     False
                     False
                     (Const.Int 64 l)
                     (Const.BitCast (Const.GlobalReference intintfunc fname) int)
    trConstr :: T.Text -> Int -> Const.Constant
    trConstr c l =
      let s = T.unpack c
      in if L.length s > 10
           then error "Syntax Error: Constructor name longer than 10 not supported"
           else if l > 3
                  then error "Syntax Error: Arguments more than 3 not supported"
                  else let hashcode = encConstr s (fromIntegral l)
                       in Const.Int 64 hashcode
    trArg :: String -> S.Set T.Text -> STG.Atom -> Integer -> [Named Instruction]
    trArg basename ref a i =
      let allocname = Name $ basename ++ "_al"
          calcname = Name $ basename ++ "_arg" ++ show i
          addr = Name $ basename ++ "_ad" ++ show i
      in [ calcname :=
           Add
             False
             False
             (LocalReference int allocname)
             (ConstantOperand (Const.Int 64 (i * 8)))
             []
         , addr := IntToPtr (LocalReference int calcname) intptr []
         , Do $
           Store
           { volatile = False
           , address = LocalReference intptr addr
           , value =
               case a of
                 STG.AtomLit (STG.Literal l) -> ConstantOperand $ trLit l
                 STG.AtomVar (STG.Var v) ->
                   let vname = Name $ T.unpack v
                   in if S.member v ref
                        then LocalReference int vname
                        else ConstantOperand $
                             Const.BitCast (Const.GlobalReference intintfunc vname) int
           , maybeAtomicity = Nothing
           , alignment = 8
           , metadata = []
           }
         ]
trFillBind _ _ _ = error "Syntax Error: Lambda expression in local binding not lifted"
    -- ^ lambda expressions should be lifted to top bindings

-- | allocate heap space for thunks created by every single local binding
trPreBind :: STG.Var -> STG.LambdaForm -> [Named Instruction]
trPreBind (STG.Var bndr) (STG.LambdaForm [] _ [] expr) =
  let bndrname = T.unpack bndr
  in case expr of
       STG.Let {} -> error "Syntax Error: Let expression in local binding not lifted"
       STG.Case _ _ -> error "Syntax Error: Case expression in local binding not lifted"
              -- ^ complex expressions are not allowed in local bindings
       STG.AppF _ as ->
         let memsize = fromIntegral $ (L.length as + 1) * 8
         in crtThunk bndrname memsize unevalFuncTag
       STG.AppC _ as ->
         let memsize = fromIntegral $ (L.length as + 1) * 8
         in crtThunk bndrname memsize evalConTag
       STG.AppP {} -> error "Syntax error: primitive operation in local binding not supported"
       STG.LitE (STG.Literal l) ->
         [ Name bndrname :=
           Add
             False
             False
             (ConstantOperand (trLit l))
             (ConstantOperand (Const.Int 64 evalLitTag))
             []
         ]
  where
    callMalloc :: Integer -> Instruction
              -- ^ call the external malloc function to allcate space
    callMalloc sz =
      Call
      { tailCallKind = Nothing
      , callingConvention = C
      , returnAttributes = []
      , function = Right (ConstantOperand (Const.GlobalReference int (Name "malloc")))
      , arguments = [(ConstantOperand $ Const.Int 64 sz, [])]
      , functionAttributes = []
      , metadata = []
      }
    crtThunk :: String -> Integer -> Integer -> [Named Instruction]
    crtThunk n s t =
      let refname = Name n
          allocname = Name $ n ++ "_al"
      in [ allocname := callMalloc s
         , refname :=
           Add
           { nsw = False
           , nuw = False
           , operand0 = LocalReference int allocname
           , operand1 = ConstantOperand $ Const.Int 64 t
           , metadata = []
           }
         ]
trPreBind _ _ = error "Syntax Error: Lambda expression in local binding not lifted"
    -- ^ lambda expressions should be lifted to top bindings

-- | translate literal number into constants
trLit :: Integer -> Const.Constant
-- ^ a literal number is stored as 3 digits shifted
trLit l = Const.Shl False False (Const.Int 64 l) (Const.Int 64 3)

-- | tranlate an top-level binding into a function,
-- which is in responsibility to update a thunk
-- into a construction or a literal
trTopBind :: STG.Var -> STG.LambdaForm -> Definition
trTopBind (STG.Var f) (STG.LambdaForm fvs _ bndrs expr) =
  GlobalDefinition
    Gbl.functionDefaults
    { Gbl.name = fname
    , Gbl.parameters = ([Parameter int (Name "ptr") []], False)
    , Gbl.returnType = int
    , Gbl.basicBlocks =
        BasicBlock (Name "entry") (initfetch ++ fetches) (Do (Br (Name "layer1") [])) :
        trBody 1 initRef expr
    , Gbl.alignment = 8
    }
  where
    fname :: Name
    fname = Name $ T.unpack f
    as :: [T.Text]
    as = map (\(STG.Var n) -> n) (fvs ++ bndrs) L.\\ [T.pack "_"]
    fetches :: [Named Instruction]
    fetches = concat (zipWith fetcharg as [1 ..])
    initfetch :: [Named Instruction]
    initfetch =
      [ Name "ptr_trim" :=
        LShr
        { exact = False
        , operand0 = LocalReference int (Name "ptr")
        , operand1 = ConstantOperand (Const.Int 64 3)
        , metadata = []
        }
      , Name "ptr_base" :=
        Shl False False (LocalReference int (Name "ptr_trim")) (ConstantOperand (Const.Int 64 3)) []
      , Name "ptr_addr0" := IntToPtr (LocalReference int (Name "ptr_base")) intptr []
      , Name "ptr_data" :=
        Load
        { volatile = False
        , address = LocalReference intptr (Name "ptr_addr0")
        , maybeAtomicity = Nothing
        , alignment = 8
        , metadata = []
        }
      , Name "ptr_tag" :=
        And (LocalReference int (Name "ptr_data")) (ConstantOperand (Const.Int 64 7)) []
      ]
    fetcharg :: T.Text -> Integer -> [Named Instruction]
    fetcharg v i =
      let n = T.unpack v
      in if n == "_"
           then []
           else [ Name ("ptr_arg" ++ show i) :=
                  Add
                    False
                    False
                    (LocalReference int (Name "ptr_base"))
                    (ConstantOperand (Const.Int 64 (i * 8)))
                    []
                , Name ("ptr_ad" ++ show i) :=
                  IntToPtr (LocalReference int (Name ("ptr_arg" ++ show i))) intptr []
                , Name n :=
                  Load
                  { volatile = False
                  , address = LocalReference int (Name ("ptr_ad" ++ show i))
                  , maybeAtomicity = Nothing
                  , alignment = 8
                  , metadata = []
                  }
                ]
    initRef :: S.Set T.Text
    initRef =
      foldr
        (\n s ->
           if T.unpack n == "_"
             then s
             else S.insert n s)
        S.empty
        as

-- | translate a functions body instructions
trBody :: Integer -> S.Set T.Text -> STG.Expr -> [BasicBlock]
trBody i ref (STG.Let _ (STG.Binds bd) innerExpr) =
  let (innerRef, instrs) = trLocalBinds ref bd
  in BasicBlock (Name ("layer" ++ show i)) instrs (Do $ Br (Name ("layer" ++ show (i + 1))) []) :
     trBody (i + 1) innerRef innerExpr
trBody i ref (STG.Case (STG.AppF (STG.Var v) []) (STG.Alts STG.NoNonDefaultAlts (STG.DefaultNotBound e))) =
  case e of
    STG.Let {} -> error "Error: Complicated alternatives not supported"
    STG.Case {} -> error "Error: Complicated alternatives not supported"
    _ -> trUpdateV i ("layer" ++ show i) ("layer" ++ show (i + 1)) (T.unpack v) ++ trBody i ref e
trBody _ _ STG.Case {} = error "Syntax Error: complicated case evaluation not supported"
trBody i ref e@STG.AppC {} =
  [ BasicBlock
      (Name ("layer" ++ show i))
      (trPreBind (STG.Var $ T.pack "retval") (STG.LambdaForm [] STG.Update [] e) ++
       trFillBind ref (STG.Var $ T.pack "retval") (STG.LambdaForm [] STG.Update [] e) ++
       [replaceThunk "ptr_addr0" "retval"])
      (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
  ]
trBody i ref e@STG.LitE {} =
  [ BasicBlock
      (Name ("layer" ++ show i))
      (trPreBind (STG.Var $ T.pack "retval") (STG.LambdaForm [] STG.Update [] e) ++
       trFillBind ref (STG.Var $ T.pack "retval") (STG.LambdaForm [] STG.Update [] e) ++
       [replaceThunk "ptr_addr0" "retval"])
      (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
  ]
trBody i ref e@(STG.AppF (STG.Var f) _) =
  [ BasicBlock
      (Name ("layer" ++ show i))
      (trPreBind (STG.Var $ T.pack ("__temp" ++ show i)) (STG.LambdaForm [] STG.Update [] e) ++
       trFillBind ref (STG.Var $ T.pack ("__temp" ++ show i)) (STG.LambdaForm [] STG.Update [] e) ++
       trUpdateAppF (T.unpack f) ("__temp" ++ show i) "retval" ++
       [replaceThunk "ptr_addr0" "retval"])
      (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
  ]
trBody i _ (STG.AppP op (STG.AtomLit (STG.Literal xl)) (STG.AtomLit (STG.Literal yl))) =
  let o = trOp op
  in [ BasicBlock
         (Name ("layer" ++ show i))
         [ Name "__val" := o (ConstantOperand (Const.Int 64 xl)) (ConstantOperand (Const.Int 64 yl))
         , Name "retval" :=
           Shl False False (LocalReference int (Name "__val")) (ConstantOperand (Const.Int 64 3)) []
         , replaceThunk "ptr_addr0" "retval"
         ]
         (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
     ]
trBody i ref (STG.AppP op (STG.AtomLit (STG.Literal xl)) (STG.AtomVar (STG.Var y)))
  | S.member y ref =
    let o = trOp op
    in trUpdateV i ("layer" ++ show i) "exit" (T.unpack y) ++
       [ BasicBlock
           (Name "exit")
           [ Name "__val" :=
             o
               (ConstantOperand (Const.Int 64 xl))
               (LocalReference int (Name (T.unpack y ++ "__updated")))
           , Name "retval" :=
             Shl
               False
               False
               (LocalReference int (Name "__val"))
               (ConstantOperand (Const.Int 64 3))
               []
           , replaceThunk "ptr_addr0" "retval"
           ]
           (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
       ]
trBody i ref (STG.AppP op (STG.AtomVar (STG.Var x)) (STG.AtomLit (STG.Literal yl)))
  | S.member x ref =
    let o = trOp op
    in trUpdateV i ("layer" ++ show i) "exit" (T.unpack x) ++
       [ BasicBlock
           (Name "exit")
           [ Name "__val" :=
             o
               (LocalReference int (Name (T.unpack x ++ "__updated")))
               (ConstantOperand (Const.Int 64 yl))
           , Name "retval" :=
             Shl
               False
               False
               (LocalReference int (Name "__val"))
               (ConstantOperand (Const.Int 64 3))
               []
           , replaceThunk "ptr_addr0" "retval"
           ]
           (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
       ]
trBody i ref (STG.AppP op (STG.AtomVar (STG.Var x)) (STG.AtomVar (STG.Var y)))
  | S.member x ref && S.member y ref =
    let o = trOp op
    in trUpdateV i ("layer" ++ show i) ("layer" ++ show (i + 1)) (T.unpack x) ++
       trUpdateV (i + 1) ("layer" ++ show (i + 1)) "exit" (T.unpack y) ++
       [ BasicBlock
           (Name "exit")
           [ Name "__val" :=
             o
               (LocalReference int (Name (T.unpack x ++ "__updated")))
               (LocalReference int (Name (T.unpack y ++ "__updated")))
           , Name "retval" :=
             Shl
               False
               False
               (LocalReference int (Name "__val"))
               (ConstantOperand (Const.Int 64 3))
               []
           , replaceThunk "ptr_addr0" "retval"
           ]
           (Do $ Ret (Just (LocalReference int (Name "retval"))) [])
       ]
trBody _ _ STG.AppP {} = error "Error: Global thunk not supported"

replaceThunk :: String -> String -> Named Instruction
replaceThunk addr val =
  Do
    Store
    { volatile = False
    , address = LocalReference intptr (Name addr)
    , value = LocalReference int (Name val)
    , maybeAtomicity = Nothing
    , alignment = 8
    , metadata = []
    }

trUpdateAppF :: String -> String -> String -> [Named Instruction]
trUpdateAppF func thk ret =
  [ Name ret :=
    Call
    { tailCallKind = Nothing
    , callingConvention = C
    , returnAttributes = []
    , function = Right (ConstantOperand (Const.GlobalReference intintfunc (Name func)))
    , arguments = [(LocalReference int (Name thk), [])]
    , functionAttributes = []
    , metadata = []
    }
  ]

trUpdateV :: Integer -> String -> String -> String -> [BasicBlock]
trUpdateV i entrylabel exitlabel v =
  let vn = v ++ "_" ++ show i
  in [ BasicBlock
         (Name entrylabel)
         [ Name (vn ++ "_tag") :=
           And (LocalReference int (Name v)) (ConstantOperand (Const.Int 64 1)) []
         ]
         (Do $
          CondBr
            (LocalReference int (Name (vn ++ "_tag")))
            (Name ("fetch" ++ show i))
            (Name ("upd" ++ show i))
            [])
     , BasicBlock
         (Name ("fetch" ++ show i))
         [ Name (vn ++ "_prim") :=
           LShr False (LocalReference int (Name v)) (ConstantOperand (Const.Int 64 3)) []
         , Name (vn ++ "_addr") :=
           Shl
             False
             False
             (LocalReference int (Name (vn ++ "_prim")))
             (ConstantOperand (Const.Int 64 3))
             []
         , Name (vn ++ "_ptr") := BitCast (LocalReference int (Name (vn ++ "_addr"))) intptr []
         , Name (vn ++ "_hd") :=
           Load
           { volatile = False
           , address = LocalReference intptr (Name (vn ++ "_ptr"))
           , maybeAtomicity = Nothing
           , alignment = 8
           , metadata = []
           }
         , Name (vn ++ "_st") :=
           And (LocalReference int (Name (vn ++ "_hd"))) (ConstantOperand (Const.Int 64 1)) []
         ]
         (Do $
          CondBr
            (LocalReference int (Name (vn ++ "_st")))
            (Name ("eval" ++ show i))
            (Name ("upd" ++ show i))
            [])
     , BasicBlock
         (Name ("eval" ++ show i))
         [ Name (vn ++ "_func_prim") :=
           LShr
             False
             (LocalReference int (Name (vn ++ "_hd")))
             (ConstantOperand (Const.Int 64 3))
             []
         , Name (vn ++ "_func_addr") :=
           Shl
             False
             False
             (LocalReference int (Name (vn ++ "_func_prim")))
             (ConstantOperand (Const.Int 64 3))
             []
         , Name (vn ++ "_func") :=
           BitCast (LocalReference int (Name (vn ++ "_func_addr"))) intintfunc []
         , Name (vn ++ "_eval") :=
           Call
           { tailCallKind = Nothing
           , callingConvention = C
           , returnAttributes = []
           , function = Right $ LocalReference intintfunc (Name (vn ++ "_func"))
           , arguments = [(LocalReference int (Name v), [])]
           , functionAttributes = []
           , metadata = []
           }
         , replaceThunk v (vn ++ "_eval")
         ]
         (Do $ Br (Name ("upd" ++ show i)) [])
     , BasicBlock
         (Name ("upd" ++ show i))
         [ Name (v ++ "__updated") :=
           Phi
             int
             [ (LocalReference int (Name v), Name entrylabel)
             , (LocalReference int (Name (vn ++ "_hd")), Name ("fetch" ++ show i))
             , (LocalReference int (Name (vn ++ "_eval")), Name ("eval" ++ show i))
             ]
             []
         ]
         (Do $ Br (Name exitlabel) [])
     ]

trOp :: STG.PrimOp -> Operand -> Operand -> Instruction
trOp STG.Add = \x y -> Add False False x y []
trOp STG.Sub = \x y -> Sub False False x y []
trOp STG.Mul = \x y -> Sub False False x y []
trOp STG.Div = \x y -> SDiv False x y []
trOp STG.Mod = \x y -> SRem x y []
trOp _ = error "Error: Primitive function not supported"

trProgram :: STG.Program -> [Definition]
trProgram (STG.Program (STG.Binds m)) = map (uncurry trTopBind) (M.assocs m)
