module MLL.Def where

import Data.Text hiding (show)

--Binary operators
data Op = Add
        | Sub
        | Mul
        | Div
        | AddEq
        | SubEq
        | MulEq
        | DivEq
        | AddOne
        | SubOne
            deriving (Read, Eq)

--Unary operators
data UOp = Neg
         | Not
            deriving (Read, Eq)

data RelOp = Eq -- == (equality)
            | Neq -- != (inequality)
            | Lt -- < (less than)
            | Le -- <= (less than or equal)
            | Gt -- > (greater than)
            | Ge -- >= (greater than or equal)
                deriving (Read, Eq)

--Expressions
data Expr = Lit Int --Literal
            | StrLit Text --String literal 
            | BoolLit Bool --Boolean literal
            | CharLit Char --Character literal
            | FloatLit Float --Float literal
            | Var Text --Variable
            | Null
            | Unary UOp Expr --Unary operation ex :( -x, !x)
            | Binary Op Expr Expr --Binary operation ex :(x + y)
            | Rel RelOp Expr Expr --Relational operation ex :(x == y)
            | Call Text [Expr] --Function call
            | Bind Bind' --Binding
                deriving (Read, Eq)

--Types
data Type = IntType
            | BoolType
            | CharType
            | StringType
            | VoidType
            | FloatType
                deriving (Read, Eq)

-- Bindings
data Bind' = Bind' { bindType :: Type
                 , bindName :: Text
                 } deriving (Read, Eq)

--Statements
data Stmt = Expr Expr --Expression
            | If Expr Stmt Stmt --If condition
            | While Expr Stmt --While loop
            | For Stmt Expr Stmt Stmt --For loop
            | StmtsReturn Expr --Return statement
            | Block [Stmt] --Block of statements
            | Assign Expr Expr --Assignment
            | FuncDef Func --Function definition
                deriving (Read, Eq)

--Function definition
data Func = Func { funcType :: Type
                 , funcName :: Text -- label
                 , funcArgs :: [Bind']
                 , funcLocals :: [Bind']
                 , funcBody :: Stmt -- compile
                 } deriving (Read, Eq)

data Import = Import Text deriving (Read, Eq)

--Program
data Program = Program [Import] [Bind'] [Func] deriving (Read, Eq)

--Show derived instances
instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/"
    show AddEq = "+="
    show SubEq = "-="
    show MulEq = "*="
    show DivEq = "/="
    show AddOne = "++"
    show SubOne = "--"

instance Show UOp where
    show Neg = "-"
    show Not = "!"

instance Show RelOp where
    show Eq = "=="
    show Neq = "!="
    show Lt = "<"
    show Le = "<="
    show Gt = ">"
    show Ge = ">="

instance Show Expr where
    show (Lit i) = show i
    show (StrLit s) = show s
    show (BoolLit b) = show b
    show (CharLit c) = show c
    show (FloatLit f) = show f
    show (Var v) = unpack v
    show Null = "null"
    show (Unary op e) = show op ++ show e
    show (Binary op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (Call f args) = unpack f ++ "(" ++ showArgs args ++ ")"
      where
        showArgs [] = ""
        showArgs [x] = show x
        showArgs (x:xs) = show x ++ ", " ++ showArgs xs
    show (Rel op e1 e2) = show e1 ++ " " ++ show op ++ " " ++ show e2
    show (Bind b) = show b

instance Show Stmt where
    show (Expr e) = show e ++ ";"
    show (If cond s1 s2) = "if (" ++ show cond ++ ") " ++ show s1 ++ " else " ++ show s2
    show (While cond s) = "while (" ++ show cond ++ ") " ++ show s
    show (For i cond inc s) = "for (" ++ show i ++ "; " ++ show cond ++ "; " ++ show inc ++ ") " ++ show s
    show (StmtsReturn e) = "return " ++ show e ++ ";"
    show (Block stmts) = "{\n" ++ showStmts stmts ++ "}"
      where
        showStmts [] = ""
        showStmts (x:xs) = show x ++ "\n" ++ showStmts xs
    show (Assign v e) = show v ++ " = " ++ show e ++ ";"
    show (FuncDef f) = show f

instance Show Type where
    show IntType = "int"
    show BoolType = "bool"
    show StringType = "string"
    show VoidType = "void"
    show CharType = "char"
    show FloatType = "float"

instance Show Bind' where
    show (Bind' t n) = show t ++ " " ++ unpack n

instance Show Func where
    show (Func t n args locals body) = show t ++ " " ++ unpack n ++ "(" ++ showArgs args ++ ") " ++ showLocals locals ++ show body ++ ""
      where
        showArgs [] = ""
        showArgs [x] = show x
        showArgs (x:xs) = show x ++ ", " ++ showArgs xs
        showLocals [] = ""
        showLocals xs = "local:\n" ++ showLocals' xs
        showLocals' [] = ""
        showLocals' (x:xs) = "    " ++ show x ++ "\n" ++ showLocals' xs

instance Show Import where
    show (Import i) = "import " ++ unpack i ++ ";"

instance Show Program where
    show (Program imp binds funcs) = showImports imp ++ showBinds binds ++ showFuncs funcs
      where
        showImports [] = ""
        showImports xs = "imports:\n" ++ showImports' xs
        showImports' [] = ""
        showImports' (x:xs) = "    " ++ show x ++ "\n" ++ showImports' xs
        showBinds [] = ""
        showBinds xs = "global:\n" ++ showBinds' xs
        showBinds' [] = ""
        showBinds' (x:xs) = "    " ++ show x ++ "\n" ++ showBinds' xs
        showFuncs [] = ""
        showFuncs xs = "\n" ++ showFuncs' xs
        showFuncs' [] = ""
        showFuncs' (x:xs) = "" ++ show x ++ "\n" ++ showFuncs' xs

-- Example program that adds two numbers
-- exampleProgram :: Program
-- exampleProgram = Program
--     []
--     [ Func IntType (pack "add") [Bind IntType (pack "x"), Bind IntType (pack "y")] []
--         (Return (Binary Add (Var (pack "x")) (Var (pack "y"))))
--     ]
