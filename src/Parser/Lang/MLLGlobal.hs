{-# LANGUAGE LambdaCase #-}

module Parser.Lang.MLLGlobal (
  parseMLLGlobalBool
  , parseMLLNumber
  , parseMLLString
  , parseMLLInlineComment
  , parseMLLMultiLineComment
  , parseMLLIdentifier
  , parseMLLIntType
  , parseMLLFunc
  , parseMLLBinaryAll
  , parseMLLBinaryPar
  , parseMLLCall
  , parseMLLExpr
  , parseMLLUnary
  , parseMLLBool
  , parseMLLExprAssign
  , parseMLLProgram
  , parseMLLStmtIf
  , parseMLLStmtsReturn
  , parseMLLBlock
  , parseMLLStmtWhile
  , parseMLLAssign
  , parseMLLGlobal
  , parseMLLStmtFor
  , parseMLLQuickAssign
  , parseMLLBindExpr
) where

import Parser.Lang.En
import Parser.Lang.Fr
import Parser.Lang.Pt
import Parser.Lang.Common
import MLL.Def
import Control.Applicative
import Parser.Lib
import Data.Text

parseMLLGlobalBool :: Parser Bool
parseMLLGlobalBool = parseMLLBoolEn <|> parseMLLBoolFr <|> parseMLLBoolPt

parseReturnValue :: Parser Expr
parseReturnValue = parseMLLBinaryAll <|> parseMLLUnary <|> parseMLLCall <|> 
           parseMLLFloat <|> parseMLLNumber <|> parseMLLString <|> 
           parseMLLIdentifier <|> parseMLLBindExpr

parseMLLStmtsReturn :: Parser Stmt
parseMLLStmtsReturn = StmtsReturn <$> 
  ((parseMLLReturnEn <|> parseMLLReturnFr <|> parseMLLReturnPt) *> 
   parseUselessChar *> parseReturnValue)

parseIfCondition :: Parser Expr
parseIfCondition = parseUselessChar *> parseString "(" *> parseUselessChar *> 
  (parseMLLRelativeExpr <|> parseMLLBool <|> parseMLLCallFull) <* 
  parseUselessChar <* parseString ")" <* parseUselessChar

simpleNullBlock :: Parser Expr
simpleNullBlock = return Null

parseElseBlock :: Parser Stmt
parseElseBlock = 
  ((parseMLLElseEn <|> parseMLLElseFr <|> parseMLLElsePt) *> 
   parseUselessChar *> parseMLLBlock) <|> (Expr <$> simpleNullBlock)

parseMLLStmtIf :: Parser Stmt
parseMLLStmtIf = do
  condition <- parseUselessChar *> (parseMLLIfEn <|> parseMLLIfFr <|> parseMLLIfPt) *> 
               parseUselessChar *> parseIfCondition
  block     <- parseUselessChar *> parseMLLBlock <* parseUselessChar
  elseBlock <- parseElseBlock
  return $ If condition block elseBlock

parseMLLStmtWhile :: Parser Stmt
parseMLLStmtWhile = do
  condition <- parseUselessChar *> 
               (parseMLLWhileEn <|> parseMLLWhileFr <|> parseMLLWhilePt) *> 
               parseUselessChar *> parseIfCondition
  block     <- parseUselessChar *> parseMLLBlock <* parseUselessChar
  return $ While condition block

parseMLLStmtFor :: Parser Stmt
parseMLLStmtFor = do
  _              <- parseUselessChar *> (parseMLLForEn <|> parseMLLForFr <|> parseMLLForPt) <* parseUselessChar
  _              <- parseString "(" <* parseUselessChar
  initialization <- parseMLLAssign <* parseUselessChar <* parseChar ';' <* parseUselessChar
  condition      <- (parseMLLRelativeExpr <|> parseMLLBool <|> parseMLLCallFull) 
               <* parseUselessChar <* parseChar ';' <* parseUselessChar
  increment      <- (parseMLLAssign <|> parseMLLAssignNew <|> parseMLLQuickAssign) <* parseUselessChar
  _              <- parseString ")" <* parseUselessChar
  block          <- parseMLLBlock <* parseUselessChar
  return $ For initialization condition increment block

parseMLLLoop :: Parser Stmt
parseMLLLoop = parseMLLStmtWhile <|> parseMLLStmtFor

parseMLLRelativeExpr :: Parser Expr
parseMLLRelativeExpr = do
  leftExpr  <- parseMLLExpr <* parseUselessChar
  op        <- parseMLLRelOp <* parseUselessChar
  rightExpr <- parseMLLExpr <* parseUselessChar
  return $ Rel op leftExpr rightExpr

parseMLLBool :: Parser Expr
parseMLLBool = BoolLit <$> parseMLLGlobalBool

parseMLLExpr :: Parser Expr
parseMLLExpr = parseUselessChar *> 
  (parseMLLFloat <|> parseMLLNumber <|> parseMLLString <|> parseMLLIdentifier <|> 
   parseMLLChar <|> parseMLLBool)

parseMLLUnary :: Parser Expr
parseMLLUnary = Unary <$> parseMLLUOp <*> parseMLLExpr

parseMLLBinaryPar :: Parser Expr
parseMLLBinaryPar = do
  _         <- parseChar '(' <* parseUselessChar
  leftExpr  <- (parseMLLBinaryPar <|> parseMLLBinary <|> parseMLLCallFull <|> parseMLLExpr) <* parseUselessChar
  op        <- parseMLLOp <* parseUselessChar
  rightExpr <- (parseMLLBinaryPar <|> parseMLLBinary <|> parseMLLCallFull <|> parseMLLExpr) <* parseUselessChar
  _         <- parseChar ')' <* parseUselessChar
  return $ Binary op leftExpr rightExpr

parseMLLBinary :: Parser Expr
parseMLLBinary = do
  leftExpr  <- (parseMLLCallFull <|> parseMLLExpr <|> parseMLLBinaryPar) <* parseUselessChar
  op        <- parseMLLOp <* parseUselessChar
  rightExpr <- (parseMLLBinary <|> parseMLLBinaryPar <|> parseMLLCallFull <|> parseMLLExpr) <* parseUselessChar
  return $ Binary op leftExpr rightExpr


parseMLLBinaryAll :: Parser Expr
parseMLLBinaryAll = parseMLLBinary <|> parseMLLBinaryPar

parseMLLBinaryExpr :: Parser Expr
parseMLLBinaryExpr = ((parseMLLExpr ) >>= \leftExpr -> 
      parseMLLOp >>= \op ->
        (parseMLLExpr ) >>= \rightExpr ->
            case op of
                _ -> return $ Binary op leftExpr rightExpr)

prependExpr :: Parser Expr -> Parser [Expr] -> Parser [Expr]
prependExpr exprParser listParser = 
  (\expr exprList -> expr : exprList) <$> exprParser <*> listParser

parseMLLCallPara :: Parser Expr
parseMLLCallPara =  parseUselessChar *> parseString "," *> (parseMLLBinaryExpr <|> parseMLLExpr)

parseMLLCallMini :: Parser [Expr]
parseMLLCallMini = prependExpr (parseMLLBinaryExpr <|> parseMLLExpr) (parseMany parseMLLCallPara)

parseMLLCallFull :: Parser Expr
parseMLLCallFull =
  Call <$>  (parseUselessChar *> parseMLLIdentifier >>= extractVar)
  <* parseUselessChar <* parseString "("
  <*> parseMLLCallMini <* parseUselessChar
  <* parseString ")" <* parseUselessChar

parseMLLCallEmpty :: Parser Expr
parseMLLCallEmpty =
  Call <$> (parseMLLIdentifier >>= \case 
        Var txt -> return txt
        _ -> error "Invalid call")
  <* parseUselessChar <* parseString "("
  <* parseUselessChar  <* parseString ")" <* parseUselessChar <*> pure [Null]

parseMLLCall :: Parser Expr
parseMLLCall = parseMLLCallEmpty <|> parseMLLCallFull

parseMLLExprAssign :: Parser Expr
parseMLLExprAssign = parseMLLBinaryAll <|> parseMLLCall <|> parseMLLUnary <|> 
            parseMLLChar <|> parseMLLBool <|> parseMLLString <|> 
            parseMLLFloat <|> parseMLLNumber <|> parseMLLIdentifier

parseMLLAllExpr :: Parser Stmt
parseMLLAllExpr = Expr <$> (parseMLLBindExpr <|> parseMLLCall <|> parseMLLBinaryAll <|> parseMLLUnary)

parseMLLAssign :: Parser Stmt
parseMLLAssign = do
  bindExpr  <- parseMLLBindExpr <* parseUselessChar <* parseChar '=' <* parseUselessChar
  expr      <- parseMLLExprAssign
  return $ Assign bindExpr expr

parseMLLAssignNew :: Parser Stmt
parseMLLAssignNew = do
  identifier <- parseMLLIdentifier <* parseUselessChar <* parseChar '=' <* parseUselessChar
  expr       <- parseMLLExprAssign
  return $ Assign identifier expr


parseMLLQuickAssign :: Parser Stmt
parseMLLQuickAssign = do
  identifier <- parseMLLIdentifier <* parseUselessChar
  assignType <- parseMLLAssignOp <* parseUselessChar
  value      <- parseMLLExprAssign
  case assignType of
    AddEq  -> return $ Assign identifier (Binary Add identifier value)
    SubEq  -> return $ Assign identifier (Binary Sub identifier value)
    MulEq  -> return $ Assign identifier (Binary Mul identifier value)
    DivEq  -> return $ Assign identifier (Binary Div identifier value)
    AddOne -> return $ Assign identifier (Binary Add identifier (Lit 1))
    SubOne -> return $ Assign identifier (Binary Sub identifier (Lit 1))
    _      -> error "Invalid assignment"

parseMLLStmtFunc :: Parser Stmt
parseMLLStmtFunc = do
  _          <- parseUselessChar
  typeParsed <- parseMLLType <* parseUselessChar
  what       <- (parseMLLIdentifier >>= extractVar) <* parseUselessChar
  parameter  <- parseMLLParameter <* parseUselessChar
  huh        <- pure []
  block      <- parseMLLBlock <* parseUselessChar 
  return $ FuncDef( Func typeParsed what parameter huh block)

parseMLLBlock :: Parser Stmt
parseMLLBlock = do
  _   <- parseChar '{' <* parseUselessChar
  res <- parseMany (
    ((parseMLLAssign <|> parseMLLAssignNew <|> parseMLLQuickAssign <|> 
      parseMLLAllExpr <|> parseMLLStmtsReturn) <* parseUselessChar <* 
      parseChar ';' <* parseUselessChar) <|> 
    (parseMLLStmtIf <|> parseMLLLoop <|> parseMLLStmtFunc) <* 
    parseUselessChar) <* parseUselessChar
  _   <- parseChar '}'
  return $ Block res

parseMLLFunc :: Parser Func
parseMLLFunc = do
  _          <- parseUselessChar
  typeParsed <- parseMLLType <* parseUselessChar
  what       <- (parseMLLIdentifier >>= extractVar) <* parseUselessChar
  parameter  <- parseMLLParameter <* parseUselessChar
  huh        <- pure []
  block      <- parseMLLBlock <* parseUselessChar 
  return $ Func typeParsed what parameter huh block

parseMLLGlobal :: Parser Stmt
parseMLLGlobal = parseString "global" *> parseUselessChar *> 
  (parseMLLAssignNew <|> (Expr <$> Bind <$> parseMLLBind)) <* 
  parseUselessChar <* parseString ";"

extractVar :: Expr -> Parser Text
extractVar (Var txt) = return txt
extractVar _ = error "var not found"

parseMLLImport :: Parser Import
parseMLLImport = do
  _    <- parseUselessChar
  _    <- parseString "#" *> (parseMLLImportEn <|> parseMLLImportFr <|> parseMLLImportPt) <* parseUselessChar
  path <- parseChar '\"' *> (pack <$> parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ 
          ['0'..'9'] ++ ['_'] ++ ['-'] ++ ['.'] ++ ['/']))) <* parseUselessChar <* parseChar '\"'
  return $ Import path

parseMLLProgram :: Parser Program 
parseMLLProgram = do
  imports <- parseMany (parseMLLImport <* parseUselessChar)
  let binds = []
  funcs <- parseManyWithError (parseMLLFunc <* parseUselessChar)
  return $ Program imports binds funcs
