module Compiler.PreCompiler where

import MLL.Def
import Data.Text hiding (map, any, elem, show)
import Data.Text.IO (readFile)
import Prelude hiding (readFile)
import Parser.Lang.MLLGlobal
import Parser.Lib
import System.IO.Error (catchIOError)


data PCState = PCState { progList :: [Program], fileName :: [Text] } deriving (Show)

initPcState :: [Program] -> [Text] -> PCState
initPcState pList fName = PCState { progList = pList, fileName = fName }

preCompileProgram :: PCState -> IO PCState
preCompileProgram pcState = expandIncludes pcState

-- parse the file that is in Program Include
expandIncludes :: PCState -> IO PCState
expandIncludes (PCState { progList = [], fileName = fName }) = return PCState { progList = [], fileName = fName }
expandIncludes (PCState { progList = (Program [] bind func) : xs, fileName = fName }) = do
    rest <- expandIncludes (PCState { progList = xs, fileName = fName })
    return PCState { progList = [Program [] bind func] ++ progList rest, fileName = fName }
expandIncludes (PCState { progList = (Program includes bind func) : xs, fileName = fName }) = do
    let includeFiles = map (\(Import i) -> i) includes
    newProgs <- if any (`elem` fName) includeFiles
                then error "Circular dependency detected"
                else mapM parseFile includeFiles
    rest <- expandIncludes (PCState { progList = newProgs ++ xs, fileName = fName ++ includeFiles })
    let combinedProgList = [Program includes bind func] ++ progList rest
    return PCState { progList = combinedProgList, fileName = includeFiles ++ fName }

parseFile :: Text -> IO Program
parseFile fName = do
    content <- catchIOError (readFile (unpack fName)) handleReadError
    case runParser parseMLLProgram (unpack content) of
        Left err -> error $ "Error while parsing file: " ++ show err
        Right (program, _) -> return program
  where
    handleReadError :: IOError -> IO Text
    handleReadError _ = do
        stdContent <- readFile ("./stdlib/" ++ unpack fName)
        return stdContent