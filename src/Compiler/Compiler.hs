{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}

module Compiler.Compiler where

import Data.Text hiding (concatMap,show)
import VM.Instruction
import MLL.Def


data CompilerState = CompilerState { forId :: Int, ifId :: Int }

type CompileEnv = ([Instruction], CompilerState)

runCompiler :: Program -> [Instruction]
runCompiler (Program _ _ funcs) =
    let (instrs, _) = compileFunction funcs ([], CompilerState 0 0)
    in instrs

compile :: [Stmt] -> CompileEnv -> CompileEnv
compile [] env = env
compile (x:xs) (instr, state) =
    let (instr', state') = compileStmt x (instr, state)
    in compile xs (instr', state')


compileFunction :: [Func] -> CompileEnv -> CompileEnv
compileFunction [] env = env
compileFunction (Func _ name args _ stmts : xs) (instrs, state) =
    let instrs' = instrs ++ [Label name]
        (argsInstrs, state') = compileArgs args (instrs', state)
        (stmtsInstrs, state'') = compileStmt stmts (argsInstrs, state')
    in compileFunction xs (stmtsInstrs, state'')


compileArgs :: [Bind'] -> CompileEnv -> CompileEnv
compileArgs [] env = env
compileArgs (x:xs) env =
    let env' = compileBind x env
    in compileArgs xs env'


pushArgs :: [Expr] -> CompileEnv -> CompileEnv
pushArgs [] env = env
pushArgs (x:xs) env =
    let env' = compileExpr x env
    in pushArgs xs env'


compileBind :: Bind' -> CompileEnv -> CompileEnv
compileBind (Bind' _ name) (instrs, state) = (instrs ++ [StoreVar name], state)


compileExpr :: Expr -> CompileEnv -> CompileEnv
compileExpr (Lit n) (instrs, state) = (instrs ++ [LoadConst (Lit n)], state)
compileExpr (StrLit s) (instrs, state) = (instrs ++ [LoadConst (StrLit s)], state)
compileExpr (BoolLit b) (instrs, state) = (instrs ++ [LoadConst (BoolLit b)], state)
compileExpr (CharLit c) (instrs, state) = (instrs ++ [LoadConst (CharLit c)], state)
compileExpr (FloatLit f) (instrs, state) = (instrs ++ [LoadConst (FloatLit f)], state)
compileExpr (Var name) (instrs, state) = (instrs ++ [LoadVar name], state)
compileExpr (Unary op expr) env =
    let (instrs, state) = compileExpr expr env
    in (instrs ++ [UnaryOp op], state)
compileExpr (Binary op expr1 expr2) env =
    let (instrs1, state1) = compileExpr expr1 env
        (instrs2, state2) = compileExpr expr2 (instrs1, state1)
    in (instrs2 ++ [BinaryOp op], state2)
compileExpr (Call name args) env =
    let env' = pushArgs args env
    in (fst env' ++ [Jmp (JmpF name)], snd env')
compileExpr (Bind (Bind' _ name)) (instrs, state) = (instrs ++ [StoreVar name], state)
compileExpr (Rel op expr1 expr2) (instrs, state) =
    let (instrs1, state1) = compileExpr expr1 (instrs, state)
        (instrs2, state2) = compileExpr expr2 (instrs1, state1)
    in (instrs2 ++ [Cmp op], state2)
compileExpr Null env = env


compileAssign :: Expr -> Expr -> CompileEnv -> CompileEnv
compileAssign (Var name) expr (env) =
    let (instrs, state) = compileExpr expr env
    in (instrs ++ [StoreVar name], state)
compileAssign (Bind (Bind' _ name)) expr env =
    let (instrs, state) = compileExpr expr env
    in (instrs ++ [StoreVar name], state)
compileAssign _ _ _ = error "Invalid assignment"


compileReturn :: Expr -> CompileEnv -> CompileEnv
compileReturn expr env =
    let (instrs, state) = compileExpr expr env
    in ( instrs ++ [Return], state)

generateLabel :: String -> CompileEnv -> (String, CompileEnv)
generateLabel prefix (instrs, CompilerState fid iid) =
    let newId = iid + 1
        newState = CompilerState fid newId
    in (prefix ++ show newId, (instrs, newState))

compileIf :: Expr -> Stmt -> Stmt -> CompileEnv -> CompileEnv
compileIf expr stmt1 (Expr Null) (instrs, CompilerState fid iid) = 
    let (exprInstrs, _) = compileExpr expr (instrs, CompilerState fid iid)
        (stmt1Instrs, _) = compileStmt stmt1 ([], CompilerState fid iid)
    in (exprInstrs
        ++ [Jmp (JmpIfNot (pack ("if_end_" ++ show iid)))]
        ++ stmt1Instrs
        ++ [Label (pack ("if_end_" ++ show iid))],
      CompilerState fid (iid + 1))

compileIf expr stmt1 stmt2 (instrs, CompilerState fid iid) = 
    let (exprInstrs, _) = compileExpr expr (instrs, CompilerState fid (iid + 1))
        (stmt1Instrs, _) = compileStmt stmt1 ([], CompilerState fid (iid + 1))
        (stmt2Instrs, _) = compileStmt stmt2 ([], CompilerState fid (iid + 1))
    in (exprInstrs
        ++ [Jmp (JmpIfNot (pack ("if_true_" ++ show iid)))]
        ++ stmt1Instrs
        ++ [Jmp (JmpIf (pack ("if_end_" ++ show iid))), Label (pack ("if_true_" ++ show iid))]
        ++ stmt2Instrs
        ++ [Label (pack ("if_end_" ++ show iid))],
      CompilerState fid (iid + 1))


compileWhile :: Expr -> Stmt -> CompileEnv -> CompileEnv
compileWhile expr stmt (instrs, CompilerState fid iid) = 
    let (exprInstrs, _) = compileExpr expr ([], CompilerState (fid + 1) iid)
        (stmtInstrs, _) = compileStmt stmt ([], CompilerState (fid + 1) (iid + 1))
    in (instrs ++ [Label (pack ("while_" ++ show iid))]
        ++ exprInstrs
        ++ [Jmp (JmpIfNot (pack ("while_end_" ++ show iid)))]
        ++ stmtInstrs
        ++ [Jmp (JmpAny (pack ("while_" ++ show iid))), Label (pack ("while_end_" ++ show iid))],
      CompilerState fid (iid + 1))

compileFor :: Stmt -> Expr -> Stmt -> Stmt -> CompileEnv -> CompileEnv
compileFor ini cond update stmt (instrs, CompilerState fid i) = 
    let (initInstrs, _) = compileStmt ini (instrs, CompilerState (fid + 1) i)
        (condInstrs, _) = compileExpr cond ([], CompilerState (fid + 1) i)
        (stmtInstrs, _) = compileStmt stmt ([], CompilerState (fid + 1) (i + 1))
        (updateInstrs, _) = compileStmt update ([], CompilerState (fid + 1) (i + 1))
    in (initInstrs
        ++ [Label (pack ("for_" ++ show i))]
        ++ condInstrs
        ++ [Jmp (JmpIfNot (pack ("for_end_" ++ show i)))]
        ++ stmtInstrs
        ++ updateInstrs
        ++ [Jmp (JmpAny (pack ("for_" ++ show i))), Label (pack ("for_end_" ++ show i))],
      CompilerState fid (i + 1))

compileStmt :: Stmt -> CompileEnv -> CompileEnv
compileStmt (Assign name expr) env = compileAssign name expr env

compileStmt (StmtsReturn expr) env = compileReturn expr env

compileStmt (Expr expr) env = compileExpr expr env

compileStmt (Block stmts) env = compile stmts env

compileStmt (If expr stmt1 stmt2) env = compileIf expr stmt1 stmt2 env

compileStmt (While expr stmt) env = compileWhile expr stmt env

-- Si la variante For existe dans Stmt :
compileStmt (For i cond inc stmt) env = compileFor i cond inc stmt env   

compileStmt (FuncDef (Func t n a l s)) env =
    compileFunction [f] env
  where 
    f = Func t (pack ("closure_" ++ (unpack n))) a l s


