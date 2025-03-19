module Runner where

import MLL.Def
import Env
import VM.Interpreter
import VM.Instruction
import Compiler.Compiler
import Compiler.PreCompiler
import Args
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BL
import Data.Text (pack)
import Parser.Lang.MLLGlobal
import Parser.Lib
import Parser.ErrorHandler
import Debug.Trace (trace)
import Control.Monad (when)

-- Execute Directly a source file
runOptExec :: Options -> IO Int
runOptExec (Options { optSource = Nothing }) = do
    putStrLn "No file to execute"
    return 1
runOptExec opts = do
    let sources = fromMaybe [] (optSource opts)
    whenVerbose $ putStrLn $ "[Executing files] -> " ++ show sources
    instr <- compileFiles sources (optVerbose opts)
    whenVerbose $ putStrLn "[Analyse label]"
    let (env, instructions') = analyseLabels instr emptyEnvironment 0
    whenVerbose $ putStrLn $ "[Label Found] -> " ++ show env
    whenVerbose $ putStrLn "[Executing]"
    let (finalEnv, finalStack, _, _) =
            execute instructions' (env, [], [], fromMaybe (error "Label not found") (getLabel (pack "main") env))
    whenVerbose $ putStrLn $ "[Done]\n[Stack] -> " ++ show finalStack
    return $ extractResult finalStack finalEnv
  where
    whenVerbose action
        | optVerbose opts = action
        | otherwise       = return ()

runOptBuild :: Options -> IO Int
runOptBuild (Options { optSource = Nothing }) = do
    putStrLn "No source file provided"
    return 1
runOptBuild opts = do
    let file = fromMaybe "a.out" (optOutput opts)
    whenVerbose $ putStrLn $ "[Building file] -> " ++ file
    instr <- compileFiles (fromMaybe [] (optSource opts)) (optVerbose opts)
    let bytecode = serializeBytecode instr
    BL.writeFile file bytecode
    putStrLn $ "File " ++ file ++ " created"
    return 0
  where
    whenVerbose
        | optVerbose opts = id
        | otherwise       = const (return ())

-- parse file
runOptParse :: Options -> IO Int
runOptParse opts = do
    let file = fromMaybe "" (optParse opts)
    putStrLn $ "Parsing file: " ++ file
    content <- readFile file
    let res = runParser parseMLLProgram content
    case res of
        Left err -> do 
            putStrLn (showError err content)
            return 1
        Right r -> do 
            print r
            return 0

-- load file for execution
runOptLoad :: Options -> IO Int
runOptLoad opts = do
    let file = fromMaybe "" (optLoad opts)
    putStrLn $ "Loading file: " ++ file
    logVerbose "[Translate file] -> " file
    bytecodeFromFile <- BL.readFile file
    logVerbose "[Deserializing]" ""
    let instructions = deserializeBytecode bytecodeFromFile
    logVerbose "[Instructions] -> " (showInstructions instructions)
    logVerbose "[Analyse label]" ""
    let (env, instructions') = analyseLabels instructions emptyEnvironment 0
    logVerbose "[Label Found] -> " (show env)
    logVerbose "[Executing]" ""
    let (finalEnv, finalStack, _, _) = execute instructions' 
                                          (env, [], [], fromMaybe (error "Label not found") (getLabel (pack "main") env))
    logVerbose "[Done]\n[Stack] -> " (show finalStack)
    return $ extractResult finalStack finalEnv
  where
    -- Fonction auxiliaire pour afficher les messages verbaux conditionnels
    logVerbose :: String -> String -> IO ()
    logVerbose message details
        | optVerbose opts = putStrLn (message ++ details)
        | otherwise = return ()

-- Fonction pour extraire le résultat de la pile finale
extractResult :: Stack -> Environment -> Int
extractResult (Lit res:_) _ = res
extractResult (CharLit c:_) _ = fromEnum c
extractResult (BoolLit b:_) _ = fromEnum b
extractResult (FloatLit f:_) _ = fromEnum f
extractResult (Var v:_) env = case getVar v env of
    Just (Lit i) -> i
    Just (CharLit c) -> fromEnum c
    Just (BoolLit b) -> fromEnum b
    Just (FloatLit f) -> fromEnum f
    _ -> -1
extractResult _ _ = -1

-- decomplie file
runOptDecompile :: Options -> IO Int
runOptDecompile opts = do
    let file = fromMaybe "" (optDecompile opts)
    putStrLn $ "Decompiling file: " ++ file
    whenVerbose "[Translate file] -> " file
    bytecodeFromFile <- BL.readFile file
    whenVerbose "[Deserializing]" ""
    let instructions = deserializeBytecode bytecodeFromFile
    whenVerbose "[Instructions] -> " (showInstructions instructions)
    let content = showInstructions instructions
    let file' = file ++ ".all"
    writeFile file' content
    putStrLn $ "File " ++ file' ++ " created"
    return 0
  where
    -- Helper function to handle verbose output
    whenVerbose :: String -> String -> IO ()
    whenVerbose msg extra
        | optVerbose opts = putStrLn $ msg ++ extra
        | otherwise       = return ()


-- Compiler files utils
compileListProgram :: [Program] -> [Instruction]
compileListProgram [] = []
compileListProgram (x:xs) = runCompiler x ++ compileListProgram xs

compileFiles :: [String] -> Bool -> IO [Instruction]
compileFiles [] _ = return []
compileFiles [file] opt = do
    content <- readFile file
    when opt $ trace ("[Compiling files] -> " ++ file) (return ())
    case runParser parseMLLProgram content of
        Left err -> do
            putStrLn (showError err content)
            return []
        Right (prog, err)
            | not (null err) -> do
                print err
                return []
            | otherwise -> do
                when opt $ trace ("[Parsing Done] -> " ++ show prog) (return ())
                precompiled <- preCompileProgram (initPcState [prog] [pack file])
                let instructions = compileListProgram (progList precompiled)
                when opt $ trace ("[Instruction] -> " ++ showInstructions instructions ++ "[Compilation Done]") (return ())
                return instructions

compileFiles (file:files) opt = do
    content <- readFile file
    when opt $ trace ("[Compiling files] -> " ++ file) (return ())
    case runParser parseMLLProgram content of
        Left err -> do
            putStrLn (showError err content)
            return []
        Right (prog, err)
            | not (null err) -> do
                print err
                return []
            | otherwise -> do
                when opt $ trace ("[Parsing Done] -> " ++ show prog) (return ())
                precompiled <- preCompileProgram (initPcState [prog] [pack file])
                let instructions = compileListProgram (progList precompiled)
                when opt $ trace (showInstructions instructions) (return ())
                instructions' <- compileFiles files opt
                return (instructions ++ instructions')



