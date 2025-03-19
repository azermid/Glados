module Env where
import qualified Data.Map as M
import Data.Text hiding (show, tail, head)


import MLL.Def (Expr)

-- Environnement : contient des variables globales et locales

data Environment = Environment
    { globals :: M.Map Text Expr     -- Variables globales
    , locals  :: [M.Map Text Expr]   -- Pile de contextes locaux
    , labels  :: M.Map Text Int      -- Labels
    } deriving (Show)

-- Pile d'exécution
type Stack = [Expr]

type PC = Int

type Contextes = (Environment, Stack, [PC], PC)

-- Initialisation de l'environnement vide
emptyEnvironment :: Environment
emptyEnvironment = Environment M.empty [] M.empty

-- Ajout d'une variable globale à l'environnement
setGlobal :: Text -> Expr -> Environment -> Environment
setGlobal name value env = env { globals = M.insert name value (globals env) }

-- Ajout d'une variable locale
setLocal :: Text -> Expr -> Environment -> Environment
setLocal name value env =
    env { locals = case locals env of
                     [] -> [M.singleton name value]
                     (ctx:rest) -> (M.insert name value ctx) : rest }

addLocal :: Environment -> Environment
addLocal env = env { locals = M.empty : locals env }

dupLocal :: Environment -> Environment
dupLocal env = env { locals = (head (locals env)) : locals env }

popLocal :: Environment -> Environment
popLocal env = env { locals = tail (locals env) }

-- Récupérer une variable
getVar :: Text -> Environment -> Maybe Expr
getVar name env = 
    case locals env of
        [] -> M.lookup name (globals env)
        (ctx:_) -> M.lookup name ctx

-- Ajouter un label
setLabel :: Text -> Int -> Environment -> Environment
setLabel name pc env = env { labels = M.insert name pc (labels env) }

-- Récupérer un label
getLabel :: Text -> Environment -> Maybe Int
getLabel label env = M.lookup label (labels env)

