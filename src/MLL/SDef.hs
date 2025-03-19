{-# LANGUAGE LambdaCase #-}

module MLL.SDef where
import qualified Data.Map as M
import Data.Text hiding (show)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (isJust)
import Data.List(find, findIndex)

-- Types des opérateurs binaires, unaires et relationnels
data SUOp = SNeg | SNot
          deriving (Read, Eq, Show)

data SOp = SAdd | SSub | SMul | SDiv
          deriving (Read, Eq, Show)

data SRelOp = Eq | SNeq | SLt | SLe | SGt | SGe
              deriving (Read, Eq, Show)

-- Expressions
data SExpr = SLit Int 
           | SStrLit Text 
           | SBoolLit Bool 
           | SCharLit Char 
           | SVar Text
           | SNull
           | SUnary SUOp SExpr 
           | SBinary SOp SExpr SExpr 
           | SCall Text [SExpr] 
           deriving (Read, Eq, Show)


-- Types des variables
data SType = SIntType 
           | SBoolType 
           | SStringType 
           | SVoidType 
           deriving (Read, Eq, Show)

-- Bindings
data Bind = Bind { bindType :: SType
                 , bindName :: Text
                 } deriving (Read, Eq, Show)

-- Fonction et programme
data SFunc = SFunc { funcType :: SType
                   , funcName :: Text
                   , funcArgs :: [Bind]
                   , funcLocals :: [Bind]
                   , funcBody :: SStmt
                   } deriving (Read, Eq)

data SStmt = SExpr SExpr
           | SIf SExpr SStmt SStmt
           | SWhile SExpr SStmt
           | For SStmt SExpr SStmt SStmt --For loop
           | SSmtsReturn SExpr
           | SBlock [SStmt]
           | SAssign Text SExpr
           deriving (Read, Eq)

-- Définition de l'environnement
data VarKind = Global | Formal | Local deriving (Show, Eq, Ord)

type Vars = M.Map (Text, VarKind) SType  -- Mapping des variables
type Funcs = M.Map Text SFunc            -- Mapping des fonctions
data Env = Env { vars :: Vars            -- Variables
               , funcs :: Funcs          -- Fonctions
               } deriving (Show)

instance Show SFunc where
    show (SFunc funcType funcName funcArgs funcLocals funcBody) =
        "SFunc { funcType = " ++ show funcType ++
        ", funcName = " ++ show funcName ++
        ", funcArgs = " ++ show funcArgs ++
        ", funcLocals = " ++ show funcLocals ++
        ", funcBody = ... }"

-- Création d'un environnement vide
emptyEnvironment :: Env
emptyEnvironment = Env M.empty M.empty

-- Vérification du programme
checkBinds :: VarKind -> [Bind] -> Env -> Either String ([Bind], Env)
checkBinds kind binds env = foldM go ([], env) binds
  where
    go (acc, env') (Bind ty name) =
        case M.lookup (name, kind) (vars env') of
            Just _  -> Left $ "Duplicate binding: " ++ show name
            Nothing ->
                let updatedEnv = env' { vars = M.insert (name, kind) ty (vars env') }
                in Right (acc ++ [Bind ty name], updatedEnv)

-- Vérification des expressions
checkExpr :: SExpr -> Env -> Either String SType
checkExpr expr env = case expr of
    SLit _      -> Right SIntType
    SStrLit _   -> Right SStringType
    SBoolLit _  -> Right SBoolType
    SCharLit _  -> Right SStringType
    SVar name   -> case M.lookup (name, Global) (vars env) of
                        Just ty -> Right ty
                        Nothing -> Left $ "Variable not found: "
                            ++ unpack name
    SNull       -> Right SVoidType

    -- Vérification des opérations unaires
    SUnary op sub   -> do
        ty <- checkExpr sub env
        case (op, ty) of
            (SNeg, SIntType)   -> Right SIntType
            (SNot, SBoolType)  -> Right SBoolType
            _                  -> Left $ "Invalid unary operation: " ++ show op ++ " on type " ++ show ty

    -- Vérification des opérations binaires
    SBinary op lhs rhs -> do
        lTy <- checkExpr lhs env
        rTy <- checkExpr rhs env
        case (op, lTy, rTy) of
            (SAdd, SIntType, SIntType) -> Right SIntType
            (SSub, SIntType, SIntType) -> Right SIntType
            (SMul, SIntType, SIntType) -> Right SIntType
            (SDiv, SIntType, SIntType) -> do
                guard (rhs /= SLit 0)
                    `mplus` Left "Division by zero"
                Right SIntType
            _ -> Left $ "Invalid binary operation: " ++ show op ++ " on types " ++ show lTy ++ " and " ++ show rTy

    -- Vérification des appels de fonctions
    SCall name args -> 
        case M.lookup name (funcs env) of
            Just func -> do
                -- Vérification du nombre d'arguments
                guard (Prelude.length args == Prelude.length (funcArgs func))
                    `mplus` Left ("Function " ++ unpack name ++ " called with incorrect number of arguments")
                
                -- Vérification des types des arguments
                argTypes <- mapM (`checkExpr` env) args
                let expectedTypes = Prelude.map bindType (funcArgs func)
                guard (argTypes == expectedTypes)
                    `mplus` Left ("Argument types mismatch in call to " ++ unpack name)
                
                -- Retourne le type de la fonction si tout est correct
                Right (funcType func)
            Nothing -> Left $ "Function not found: " ++ unpack name


-- Vérification des instructions
checkStmt :: SStmt -> Env -> Either String ()
checkStmt stmt env = case stmt of
    SExpr expr -> do
        _ <- checkExpr expr env
        Right ()
    SIf cond s1 s2 -> do
        ty <- checkExpr cond env
        guard (ty == SBoolType)
            `mplus` Left "If condition must be of type bool"
        checkStmt s1 env
        checkStmt s2 env
    SWhile cond s -> do
        ty <- checkExpr cond env
        guard (ty == SBoolType)
            `mplus` Left "While condition must be of type bool"
        checkStmt s env
    SFor init cond update s -> do
        checkStmt (SExpr init) env
        ty <- checkExpr cond env
        guard (ty == SBoolType)
            `mplus` Left "For condition must be of type bool"
        checkStmt (SExpr update) env
        checkStmt s env
    SSmtsReturn expr -> do
        ty <- checkExpr expr env
        case M.lookup (pack "@returnType", Global) (vars env) of
            Just expectedTy -> guard (ty == expectedTy)
                                `mplus` Left "Return type mismatch"
            Nothing -> Left "No return type defined in current context"
    SBlock stmts -> do
        let env' = env { vars = vars env }  -- Garde les variables globales
        foldM (\_ s -> checkStmt s env') () stmts
        Right ()
    SAssign name expr -> do
        ty <- checkExpr expr env
        case M.lookup (name, Global) (vars env) of
            Just expectedTy -> guard (ty == expectedTy)
                                `mplus` Left "Assignment type mismatch"
            Nothing -> Left $ "Variable not found: " ++ unpack name
        Right ()

-- Vérification des fonctions
checkFunc :: SFunc -> Env -> Either String ()
checkFunc (SFunc ty name args locals body) env = do
    let env' = env { vars = M.empty }
    let env'' = env' { vars = M.fromList $ Prelude.map (\(Bind ty name) -> ((name, Formal), ty)) args }
    let env''' = env'' { vars = M.fromList $ Prelude.map (\(Bind ty name) -> ((name, Local), ty)) locals }
    checkStmt body env'''
    Right ()
