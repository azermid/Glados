{-# LANGUAGE OverloadedStrings #-}

module MLLGlobalSpec (spec) where

import Test.Hspec
import Parser.Lang.MLLGlobal
import MLL.Def
import Parser.Lib
import Data.Text (Text)

spec :: Spec
spec = do
  describe "parseMLLGlobalBool" $ do
    it "parses English True" $ do
      runParser parseMLLGlobalBool "True" `shouldBe` Right (True, "f")

    it "parses French True" $ do
      runParser parseMLLGlobalBool "Vrai" `shouldBe` Right (True, "")

    it "parses English false" $ do
      runParser parseMLLGlobalBool "False" `shouldBe` Right (False, "")

    it "parses French False" $ do
      runParser parseMLLGlobalBool "Faux" `shouldBe` Right (False, "")

    it "parses Portuguese True" $ do
      runParser parseMLLGlobalBool "Verdadeiro" `shouldBe` Right (True, "")
    
    it "parses Portuguese False" $ do
      runParser parseMLLGlobalBool "Falso" `shouldBe` Right (False, "")

    it "fails on invalid boolean values" $ do
      runParser parseMLLGlobalBool "invalid" `shouldBe` (Left (Missing ("if", "nvalid", "parseString")))

  describe "parseMLLStmtsReturn" $ do
    it "parses a return statement in English with a value" $ do
      let input = "return 42"
      let expected = Right (StmtsReturn (Lit 42), "")
      runParser parseMLLStmtsReturn input `shouldBe` expected

    it "parses a return statement in French with a value" $ do
      let input = "retour 42"
      let expected = Right (StmtsReturn (Lit 42), "")
      runParser parseMLLStmtsReturn input `shouldBe` expected
    
    it "parses a return statement in Portuguese with a value" $ do
      let input = "retornar 42"
      let expected = Right (StmtsReturn (Lit 42), "")
      runParser parseMLLStmtsReturn input `shouldBe` expected

    it "parses a return statement with a string value" $ do
      let input = "return \"hello\""
      let expected = Right (StmtsReturn (StrLit "hello"), "")
      runParser parseMLLStmtsReturn input `shouldBe` expected

    it "parses a return statement with an expression" $ do
      let input = "return 1 + 2"
      let expected = Right (StmtsReturn (Binary Add (Lit 1) (Lit 2)), "")
      runParser parseMLLStmtsReturn input `shouldBe` expected

    it "fails on invalid input" $ do
      let input = "retur 42"
      let expected = Left (Missing ("ret","ur 42", "parseString"))
      runParser parseMLLStmtsReturn input `shouldBe` expected

    it "fails when return keyword is missing" $ do
      let input = "42"
      let expected = Left (Missing ("42","retornar", "parseAnyChar -> parseString"))
      runParser parseMLLStmtsReturn input `shouldBe` expected

  describe "parseMLLBool" $ do
    it "parses True as BoolLit" $ do
      runParser parseMLLBool "True" `shouldBe` Right (BoolLit True, "")

    it "parses False as BoolLit" $ do
      runParser parseMLLBool "False" `shouldBe` Right (BoolLit False, "")
    
    it "parses Vrai as BoolLit" $ do
      runParser parseMLLBool "Vrai" `shouldBe` Right (BoolLit True, "")
    
    it "parses Faux as BoolLit" $ do
      runParser parseMLLBool "Faux" `shouldBe` Right (BoolLit False, "")
    
    it "parses Verdadeiro as BoolLit" $ do
      runParser parseMLLBool "Verdadeiro" `shouldBe` Right (BoolLit True, "")
    
    it "parses Falso as BoolLit" $ do
      runParser parseMLLBool "Falso" `shouldBe` Right (BoolLit False, "")

    it "fails on invalid boolean values" $ do
      runParser parseMLLBool "123" `shouldBe` (Left (Missing ("123", "Verdadeiro","parseAnyChar -> parseString")))

  describe "parseMLLExpr" $ do
    it "parses a number" $ do
      runParser parseMLLExpr "123" `shouldBe` Right (Lit 123, "")

    it "parses a string" $ do
      runParser parseMLLExpr "\"hello\"" `shouldBe` Right (StrLit "hello", "")

    it "parses an identifier" $ do
      runParser parseMLLExpr "myVar" `shouldBe` Right (Var "myVar", "")

  describe "parseMLLUnary" $ do
    it "parses negation of a number" $ do
      runParser parseMLLUnary "-123" `shouldBe` Right (Unary Neg (Lit 123), "")

    it "fails on invalid unary expression" $ do
      runParser parseMLLUnary "~123" `shouldBe` Left (Missing ("~123", "-", "parseAnyChar -> parseString"))

  describe "parseMLLBinary" $ do
    it "parses a simple binary expression with addition" $ do
      runParser parseMLLBinaryAll "1 + 2" `shouldBe` Right (Binary Add (Lit 1) (Lit 2), "")

    it "parses a binary expression with subtraction" $ do
      runParser parseMLLBinaryAll "3 - 4" `shouldBe` Right (Binary Sub (Lit 3) (Lit 4), "")

    it "parses a binary expression with multiplication" $ do
      runParser parseMLLBinaryAll "5 * 6" `shouldBe` Right (Binary Mul (Lit 5) (Lit 6), "")

    it "parses a binary expression with division" $ do
      runParser parseMLLBinaryAll "7 / 8" `shouldBe` Right (Binary Div (Lit 7) (Lit 8), "")

    it "fails on an invalid binary expression" $ do
      runParser parseMLLBinaryAll "(9 & 10)" `shouldBe` (Left (Missing ("& 10)","/","parseAnyChar -> parseString")))

    it "parses when missing parentheses" $ do
      runParser parseMLLBinaryAll "1 + 2" `shouldBe` Right (Binary Add (Lit 1) (Lit 2), "")

  describe "parseMLLCall" $ do
    it "parses a function call with no arguments" $ do
      runParser parseMLLCall "myFunc()" `shouldBe` Right (Call "myFunc" [Null], "")

    it "parses a function call with one argument" $ do
      runParser parseMLLCall "myFunc(42)" `shouldBe` Right (Call "myFunc" [Lit 42], "")

    it "handles extra spaces and useless characters" $ do
      runParser parseMLLCall "  myFunc( 42 , \"test\" )  "
        `shouldBe` Right (Call "myFunc" [Lit 42, StrLit "test"], "")

    it "fails on an invalid function call" $ do
      runParser parseMLLCall "myFunc[42]" `shouldBe` (Left (Missing ("[42]", "(", "parseAnyChar -> parseString")))

    it "fails when missing parentheses" $ do
      runParser parseMLLCall "myFunc 42" `shouldBe` (Left (Missing ("42","(","parseAnyChar -> parseString")))
    
  describe "parseMLLExprAssign" $ do
    it "parses a binary expression with addition" $ do
      runParser parseMLLExprAssign "3 + 4"
        `shouldBe` Right (Binary Add (Lit 3) (Lit 4), "")

    it "parses a unary expression with negation" $ do
      runParser parseMLLExprAssign "-42"
        `shouldBe` Right (Unary Neg (Lit 42), "")

    it "parses a character literal" $ do
      runParser parseMLLExprAssign "'c'"
        `shouldBe` Right (CharLit 'c', "")

    it "parses a boolean literal (True)" $ do
      runParser parseMLLExprAssign "True"
        `shouldBe` Right (BoolLit True, "")

    it "parses a boolean literal (False)" $ do
      runParser parseMLLExprAssign "False"
        `shouldBe` Right (BoolLit False, "")

    it "parses a string literal" $ do
      runParser parseMLLExprAssign "\"hello\""
        `shouldBe` Right (StrLit "hello", "")

    it "parses a number literal" $ do
      runParser parseMLLExprAssign "123"
        `shouldBe` Right (Lit 123, "")

    it "handles spaces and useless characters correctly" $ do
      runParser parseMLLExprAssign "    5 * 6    "
        `shouldBe` Right (Binary Mul (Lit 5) (Lit 6), "")
    
  describe "parseMLLBlock" $ do
    it "parses a block with no statements" $ do
      runParser parseMLLBlock "{}"
        `shouldBe` Right (Block [], "")

    it "parses a block with one statement" $ do
      runParser parseMLLBlock "{ return 42; }"
        `shouldBe` Right (Block [StmtsReturn (Lit 42)], "")

  describe "parseMLLFunc" $ do
    it "parses a function definition with no arguments" $ do
      runParser parseMLLFunc "int myFunc() { return 42; }"
        `shouldBe` Right (Func IntType "myFunc" [] [] (Block [StmtsReturn (Lit 42)]), "")
    
    it "parses a function definition with one argument" $ do
      runParser parseMLLFunc "\nint myFunc(int x) { return x; }"
        `shouldBe` Right (Func IntType "myFunc" [Bind' IntType "x"] [] (Block [StmtsReturn (Var "x")]), "")
    
    it "parses a function definition with multiple arguments" $ do
      runParser parseMLLFunc "int myFunc(int x, string y) { return x; }"
        `shouldBe` Right (Func IntType "myFunc" [Bind' IntType "x", Bind' StringType "y"] [] (Block [StmtsReturn (Var "x")]), "")
  
  describe "parseMLLGlobal" $ do
    it "parses a simple global definition" $ do
      let input = "global int x;"
          expected = (Expr (Bind (Bind' IntType "x")), "")
      runParser parseMLLGlobal input
        `shouldBe` Right expected
  describe "parseMLLProgram" $ do
    it "parses a program with multiple binds and funcs" $ do
      let input = unlines
            [ 
             "int myFunc(int z) { return z + x; }\nint printY() { println(y); }"
            ]
      let expectedProgram = Program
            []
            [ ]
            [ Func
                IntType "myFunc" [Bind' IntType "z"] []
                (Block [StmtsReturn (Binary Add (Var "z") (Var "x"))])
            , Func
                IntType "printY" [] []
                (Block [Expr (Call "println" [Var "y"])])
            ]
      case runParser parseMLLProgram input of
        Left err -> expectationFailure $ "Parsing failed with error: " ++ show err
        Right (program, _) -> program `shouldBe` expectedProgram
