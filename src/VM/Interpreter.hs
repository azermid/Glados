module VM.Interpreter (
    analyseLabels,
    executeInstruction,
    executeStoreVar,
    executeLoadVar,
    executeOp,
    executeUnary,
    executeCmp,
    execute
) where

import Data.Maybe (fromMaybe)
import Data.Text hiding (show, length)
import MLL.Def
import Env
import VM.Instruction

-- Analyse des labels
analyseLabels :: [Instruction] -> Environment -> Int -> (Environment, [Instruction])
analyseLabels [] env _ = (env, [])
analyseLabels (Label name:instrs) env pc = 
    let (env', instrs') = analyseLabels instrs (setLabel name pc env) pc
    in (env', instrs')
analyseLabels (instr:instrs) env pc = 
    let (env', instrs') = analyseLabels instrs env (pc + 1)
    in (env', instr : instrs')

-- Exécution d'une instruction
executeInstruction :: Instruction -> Contextes -> Contextes
executeInstruction (LoadConst n) (env, stack, callStack, pc) = 
    (env, n : stack, callStack, pc)
executeInstruction (StoreVar name) state = executeStoreVar name state
executeInstruction (LoadVar name) state = executeLoadVar name state
executeInstruction (BinaryOp op) state = executeOp op state
executeInstruction (UnaryOp op) state = executeUnary op state
executeInstruction (Cmp op) state = executeCmp op state
executeInstruction (Jmp j) state = executeJmp j state
executeInstruction Return state = executeReturn state
executeInstruction instr state = 
    error $ "Invalid instruction: " ++ show instr ++ " state " ++ show state


executeStoreVar :: Text -> Contextes -> Contextes
executeStoreVar name (env, value:stack, callStack, pc) = 
    (setLocal name value env, stack, callStack, pc)
executeStoreVar name (_, [], _, _) = 
    error $ "No value to store in variable: " ++ show name

executeLoadVar :: Text -> Contextes -> Contextes
executeLoadVar name (env, stack, callStack, pc) = 
    case getVar name env of
        Just value -> (env, value : stack, callStack, pc)
        Nothing    -> error $ "Variable not found: " ++ show name

executeOp :: Op -> Contextes -> Contextes
executeOp Add (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (x + y)) : stack, callStack, pc)
executeOp Add (env, (Var y):(Var x):stack, callStack, pc) = 
    case (getVar x env, getVar y env) of
        (Just (Lit val1), Just (Lit val2)) -> 
            (env, (Lit (val1 + val2)) : stack, callStack, pc)
        _ -> error "Variable not found or not a literal"
executeOp Add (env, (Lit y):(Var x):stack, callStack, pc) = 
    case getVar x env of
        Just (Lit val) -> (env, (Lit (val + y)) : stack, callStack, pc)
        _              -> error "Variable not found or not a literal"
executeOp Add (env, (Var y):(Lit x):stack, callStack, pc) = 
    case getVar y env of
        Just (Lit val) -> (env, (Lit (val + x)) : stack, callStack, pc)
        _              -> error "Variable not found or not a literal"
executeOp Add (env, (Lit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x + fromIntegral y)) : stack, callStack, pc)
executeOp Add (env, (FloatLit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (round (fromIntegral x + y))) : stack, callStack, pc)
executeOp Add (env, (FloatLit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x + y)) : stack, callStack, pc)
executeOp Sub (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (x - y)) : stack, callStack, pc)
executeOp Sub (env, (Lit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x - fromIntegral y)) : stack, callStack, pc)
executeOp Sub (env, (FloatLit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (round (fromIntegral x - y))) : stack, callStack, pc)
executeOp Sub (env, (FloatLit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x - y)) : stack, callStack, pc)
executeOp Mul (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (x * y)) : stack, callStack, pc)
executeOp Mul (env, (FloatLit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x * y)) : stack, callStack, pc)
executeOp Mul (env, (FloatLit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (round (fromIntegral x * y))) : stack, callStack, pc)
executeOp Mul (env, (Lit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x * fromIntegral y)) : stack, callStack, pc)
executeOp Div (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (x `div` y)) : stack, callStack, pc)
executeOp Div (env, (FloatLit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x / y)) : stack, callStack, pc)
executeOp Div (env, (FloatLit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (round (fromIntegral x / y))) : stack, callStack, pc)
executeOp Div (env, (Lit y):(FloatLit x):stack, callStack, pc) = 
    (env, (FloatLit (x / fromIntegral y)) : stack, callStack, pc)
executeOp op state = 
    error $ "Invalid operation: " ++ show op ++ " state " ++ show state

executeUnary :: UOp -> Contextes -> Contextes
executeUnary Neg (env, (Lit x):stack, callStack, pc) = 
    (env, (Lit (-x)) : stack, callStack, pc)
executeUnary Not (env, (Lit x):stack, callStack, pc) = 
    (env, (Lit (if x == 0 then 1 else 0)) : stack, callStack, pc)
executeUnary op state = 
    error $ "Invalid unary operation: " ++ show op ++ " state " ++ show state

executeCmp :: RelOp -> Contextes -> Contextes
executeCmp Eq (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x == y then 1 else 0)) : stack, callStack, pc)
executeCmp Neq (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x /= y then 1 else 0)) : stack, callStack, pc)
executeCmp Lt (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x < y then 1 else 0)) : stack, callStack, pc)
executeCmp Le (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x <= y then 1 else 0)) : stack, callStack, pc)
executeCmp Gt (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x > y then 1 else 0)) : stack, callStack, pc)
executeCmp Ge (env, (Lit y):(Lit x):stack, callStack, pc) = 
    (env, (Lit (if x >= y then 1 else 0)) : stack, callStack, pc)
executeCmp op state = 
    error $ "Invalid comparison: " ++ show op ++ " state " ++ show state

executeJmp :: Jmp -> Contextes -> Contextes
executeJmp (JmpF name) (env, stack, callStack, pc) = 
    case getLabel (pack "closure_" `append` name) env of
        Just pco -> (env, stack, pc : callStack, pco)
        Nothing  -> (addLocal env, stack, pc : callStack, 
                     fromMaybe (error "Label not found") (getLabel name env))
executeJmp (JmpAny name) (env, stack, callStack, _) = 
    (env, stack, callStack, 
     fromMaybe (error "Label not found") (getLabel name env))
executeJmp (JmpIf name) (env, (Lit 1):stack, callStack, pc)
    | (pack "if_true_") `isPrefixOf` name = (env, (Lit 1):stack, callStack, pc)
executeJmp (JmpIf name) (env, (Lit 1):stack, callStack, _) = 
    (env, stack, callStack, 
     fromMaybe (error "Label not found") (getLabel name env))
executeJmp (JmpIf _) (env, (Lit 0):stack, callStack, pc) = 
    (env, stack, callStack, pc)
executeJmp (JmpIfNot name) (env, (Lit 1):stack, callStack, pc)
    | (pack "if_true_") `isPrefixOf` name = (env, stack, callStack, pc)
executeJmp (JmpIfNot _) (env, (Lit 1):stack, callStack, pc) = 
    (env, stack, callStack, pc)
executeJmp (JmpIfNot name) (env, (Lit 0):stack, callStack, _) = 
    (env, stack, callStack, 
     fromMaybe (error "Label not found") (getLabel name env))
executeJmp jmp _ = 
    error $ "Invalid jump: " ++ show jmp

executeReturn :: Contextes -> Contextes
executeReturn (env, value:stack, [], _) = 
    (env, value : stack, [], -1)
executeReturn (env, [] ,[], _) = 
    (env, [Lit 0], [], -1)
executeReturn (env, value:stack, pco:callStack, _) = 
    (popLocal env, value : stack, callStack, pco)
executeReturn _ = error "Invalid return"

-- Exécution du programme
execute :: [Instruction] -> Contextes -> Contextes
execute [] state = state
execute _ (env, stack, callStack, -1) = (env, stack, callStack, -1)
execute instructions (env, stack, callStack, pc)
    | pc == -1 = (env, stack, callStack, pc)
    | pc < 0 || pc >= length instructions = 
        (env, stack, callStack, pc)  -- Check for out-of-bounds pc
    | otherwise = execute instructions' state'
  where
    instruction = instructions !! pc
    state' = executeInstruction instruction (env, stack, callStack, pc + 1)
    instructions' = instructions
