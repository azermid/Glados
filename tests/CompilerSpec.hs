{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
module CompilerSpec where

import Test.Hspec
import Compiler.Compiler
import MLL.Def
import VM.Instruction
import Data.Text (pack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "compile" $ do
        it "compiles an empty list of statements" $ do
            compile [] `shouldBe` []

        it "compiles a single assignment" $ do
            let stmts = [Assign (Var (pack "x")) (Lit 42)]
            compile stmts `shouldBe` [LoadConst (Lit 42), StoreVar (pack "x")]

        it "compiles a return statement" $ do
            let stmts = [SmtsReturn (Lit 42)]
            compile stmts `shouldBe` [LoadConst (Lit 42), Return]

        it "compiles a block of statements" $ do
            let stmts = [Block [Assign (Var (pack "x")) (Lit 42), SmtsReturn (Var (pack "x"))]]
            compile stmts `shouldBe` [LoadConst (Lit 42), StoreVar (pack "x"), LoadVar (pack "x"), Return]

    describe "compileFunction" $ do
        it "compiles an empty list of functions" $ do
            compileFunction [] `shouldBe` []

        it "compiles a single function" $ do
            let funcs = [Func IntType (pack "main") [] [] (Block [SmtsReturn (Lit 42)])]
            compileFunction funcs `shouldBe` [Label (pack "main"), LoadConst (Lit 42), Return]

        it "compiles a function with arguments" $ do
            let funcs = [Func IntType (pack "add") [Bind' IntType (pack "a"), Bind' IntType (pack "b")] [] (Block [SmtsReturn (Binary Add (Var (pack "a")) (Var (pack "b")))])]
            compileFunction funcs `shouldBe` [Label (pack "add"), StoreVar (pack "a"), StoreVar (pack "b"), LoadVar (pack "a"), LoadVar (pack "b"), BinaryOp Add, Return]

    describe "compileExpr" $ do
        it "compiles a literal expression" $ do
            compileExpr (Lit 42) `shouldBe` [LoadConst (Lit 42)]

        it "compiles a string literal expression" $ do
            compileExpr (StrLit (pack "foo")) `shouldBe` [LoadConst (StrLit (pack "foo"))]

        it "compiles a boolean literal expression" $ do
            compileExpr (BoolLit True) `shouldBe` [LoadConst (BoolLit True)]
        
        it "compiles a character literal expression" $ do
            compileExpr (CharLit 'a') `shouldBe` [LoadConst (CharLit 'a')]

        it "compiles a variable expression" $ do
            compileExpr (Var (pack "x")) `shouldBe` [LoadVar (pack "x")]

        it "compiles a unary expression" $ do
            compileExpr (Unary Neg (Lit 42)) `shouldBe` [LoadConst (Lit 42), UnaryOp Neg]

        it "compiles a binary expression" $ do
            compileExpr (Binary Add (Lit 1) (Lit 2)) `shouldBe` [LoadConst (Lit 1), LoadConst (Lit 2), BinaryOp Add]

        it "compiles a function call expression" $ do
            compileExpr (Call (pack "add") [Lit 1, Lit 2]) `shouldBe` [LoadConst (Lit 1), LoadConst (Lit 2), Jmp JmpF (pack "add")]

        it "compiles a binding expression" $ do
            compileExpr (Bind (Bind' IntType (pack "x"))) `shouldBe` [StoreVar (pack "x")]

    describe "compileAssign" $ do
        it "compiles an assignment to a variable" $ do
            compileAssign (Var (pack "x")) (Lit 42) `shouldBe` [LoadConst (Lit 42), StoreVar (pack "x")]

        it "compiles an assignment to a binding" $ do
            compileAssign (Bind (Bind' IntType (pack "x"))) (Lit 42) `shouldBe` [LoadConst (Lit 42), StoreVar (pack "x")]
