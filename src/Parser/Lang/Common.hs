{-# LANGUAGE LambdaCase #-}

module Parser.Lang.Common where

import Parser.Lib
import MLL.Def
import Control.Applicative
import Data.Text

parseMLLEq :: Parser RelOp
parseMLLEq = parseString "==" *> parseUselessChar *> pure Eq

parseMLLNeq :: Parser RelOp
parseMLLNeq = parseString "!=" *> parseUselessChar *> pure Neq

parseMLLLt :: Parser RelOp
parseMLLLt = parseString "<" *> parseUselessChar *> pure Lt

parseMLLLe :: Parser RelOp
parseMLLLe = parseString "<=" *> parseUselessChar *> pure Le

parseMLLGt :: Parser RelOp
parseMLLGt = parseString ">" *> parseUselessChar *> pure Gt

parseMLLGe :: Parser RelOp
parseMLLGe = parseString ">=" *> parseUselessChar *> pure Ge

parseMLLRelOp :: Parser RelOp
parseMLLRelOp = parseMLLEq <|> parseMLLNeq <|> parseMLLLe <|> parseMLLLt <|> parseMLLGe <|> parseMLLGt

parseMLLAdd :: Parser Op
parseMLLAdd = parseString "+" *> parseUselessChar *> pure Add

parseMLLSub :: Parser Op
parseMLLSub = parseString "-" *> parseUselessChar *> pure Sub

parseMLLMul :: Parser Op
parseMLLMul = parseString "*" *> parseUselessChar *> pure Mul

parseMLLDiv :: Parser Op
parseMLLDiv = parseString "/" *> parseUselessChar *> pure Div

parseMLLAddEq :: Parser Op
parseMLLAddEq = parseString "+=" *> parseUselessChar *> pure AddEq

parseMLLSubEq :: Parser Op
parseMLLSubEq = parseString "-=" *> parseUselessChar *> pure SubEq

parseMLLMulEq :: Parser Op
parseMLLMulEq = parseString "*=" *> parseUselessChar *> pure MulEq

parseMLLDivEq :: Parser Op
parseMLLDivEq = parseString "/=" *> parseUselessChar *> pure DivEq

parseMLLAddOne :: Parser Op
parseMLLAddOne = parseString "++" *> parseUselessChar *> pure AddOne

parseMLLSubOne :: Parser Op
parseMLLSubOne = parseString "--" *> parseUselessChar *> pure SubOne

parseMLLOp :: Parser Op
parseMLLOp =  parseUselessChar *> (parseMLLAdd <|> parseMLLSub <|> parseMLLMul <|> parseMLLDiv)

parseMLLAssignOp :: Parser Op
parseMLLAssignOp = parseUselessChar *> 
  (parseMLLAddEq <|> parseMLLSubEq <|> parseMLLMulEq <|> parseMLLDivEq <|> 
   parseMLLAddOne <|> parseMLLSubOne)

parseMLLNeg :: Parser UOp
parseMLLNeg = parseString "-" *> parseUselessChar *> pure Neg

parseMLLNot :: Parser UOp
parseMLLNot = parseString "!" *> parseUselessChar *> pure Not

parseMLLUOp :: Parser UOp
parseMLLUOp = parseMLLNot <|> parseMLLNeg

parseMLLInlineComment :: Parser [Char]
parseMLLInlineComment = parseUselessChar *> parseString "//" *> parseMany (parseAnyChar [' '..'~']) <* parseChar '\n'

parseMLLMultiLineComment :: Parser [Char]
parseMLLMultiLineComment = 
  parseUselessChar *> parseString "/*" *> 
  (parseMany (parseAnyChar ([' '..')'] ++ ['+'..'.'] ++ ['0'..'~'] ++ ['\n', '\t']))) <* 
  parseString "*/"

parseMLLComment :: Parser [Char]
parseMLLComment = parseMLLInlineComment <|> parseMLLMultiLineComment

parseMLLNumber :: Parser Expr
parseMLLNumber = (Lit <$> parseInt)

parseMLLFloat :: Parser Expr
parseMLLFloat = (FloatLit <$> parseFloat)

convertToTextParser :: Parser [Char] -> Parser Text
convertToTextParser parser = pack <$> parser

parseMLLString :: Parser Expr
parseMLLString = parseString "\"" *> 
  (StrLit <$> pack <$> parseMany (parseAnyChar ([' '..'!'] ++ ['#'..'~']))) <* 
  parseString "\""

parseMLLChar :: Parser Expr
parseMLLChar = parseString "\'" *> 
  (CharLit <$> parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'] ++ ['-'])) <* 
  parseString "\'"

parseMLLIdentifier :: Parser Expr
parseMLLIdentifier = parseUselessChar *> 
  (Var <$> pack <$> parseMany (parseAnyChar (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['_'] ++ ['-']))) 
  <* parseUselessChar

parseMLLBindExpr :: Parser Expr
parseMLLBindExpr = Bind <$> parseMLLBind

parseMLLIntType :: Parser Type
parseMLLIntType = parseString "int" *> parseUselessChar *> pure IntType

parseMLLCharType :: Parser Type
parseMLLCharType = parseString "char" *> parseUselessChar *> pure CharType

parseMLLStringType :: Parser Type
parseMLLStringType = parseString "string" *> parseUselessChar *> pure StringType

parseMLLVoidType :: Parser Type
parseMLLVoidType = parseString "void *" *> parseUselessChar *> pure VoidType

parseMLLBoolType :: Parser Type
parseMLLBoolType = parseString "bool" *> parseUselessChar *> pure BoolType

parseMLLFloatType :: Parser Type
parseMLLFloatType = parseString "float"*> parseUselessChar *> pure FloatType

parseMLLType :: Parser Type
parseMLLType = parseMLLBoolType <|> parseMLLVoidType <|> parseMLLStringType <|> 
         parseMLLCharType <|> parseMLLIntType <|> parseMLLFloatType

parseMLLBind :: Parser Bind'
parseMLLBind =
  parseMLLType >>= \typ ->
    parseMLLIdentifier >>= \case
      Var txt -> return $ Bind' typ txt
      _ -> error "var not found"

parseMLLParameter :: Parser [Bind']
parseMLLParameter =
  parseString "(" *> 
  parseMany (parseUselessChar *> parseMLLBind <* parseUselessChar <* optional (parseChar ',')) <* 
  parseString ")" <* 
  parseUselessChar
