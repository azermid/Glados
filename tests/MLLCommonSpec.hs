{-# LANGUAGE OverloadedStrings #-}

module MLLCommonSpec (spec) where

import Test.Hspec
import Parser.Lang.Common
import MLL.Def
import Parser.Lib
import Data.Text (Text)

spec :: Spec
spec = do
  describe "parseMLLNumber" $ do
    it "parses integers" $ do
      runParser parseMLLNumber "123" `shouldBe` Right (Lit 123, "")

    it "fails on non-numeric input" $ do
      runParser parseMLLNumber "abc" `shouldBe` Left (Missing ("-","abc","parseChar"))

  describe "parseMLLString" $ do
    it "parses quoted strings" $ do
      runParser parseMLLString "\"hello\"" `shouldBe` Right (StrLit "hello", "")

  describe "parseMLLString" $ do
    it "fails without closing quotes" $ do
      let result = runParser parseMLLString "\"hello"
      let expected = Left (Missing ("_","\"","parseAnyChar -> parseString"))
      result `shouldBe` expected
      print result

  describe "parseMLLChar" $ do
    it "parses valid characters" $ do
      runParser parseMLLChar "'a'" `shouldBe` Right (CharLit 'a', "")

    it "fails on invalid character input" $ do
      runParser parseMLLChar "'ab'" `shouldBe` Left (Missing ("b'","'","parseAnyChar -> parseString"))

  describe "parseMLLIdentifier" $ do
    it "parses valid identifiers" $ do
      runParser parseMLLIdentifier "myVar" `shouldBe` Right (Var "myVar", "")

    it "handles leading spaces" $ do
      runParser parseMLLIdentifier "   myVar" `shouldBe` Right (Var "myVar", "")

    it "handles identifiers with numbers and lettres" $ do
      runParser parseMLLIdentifier "123abc" `shouldBe` Right (Var "123abc","")

  describe "parseMLLType" $ do
    it "parses int type" $ do
      runParser parseMLLType "int" `shouldBe` Right (IntType, "")

    it "parses string type" $ do
      runParser parseMLLType "string" `shouldBe` Right (StringType, "")

    it "parses bool type" $ do
      runParser parseMLLType "bool" `shouldBe` Right (BoolType, "")

  describe "parseMLLRelOp" $ do
    it "parses ==" $ do
      runParser parseMLLRelOp "==" `shouldBe` Right (Eq, "")

    it "parses !=" $ do
      runParser parseMLLRelOp "!=" `shouldBe` Right (Neq, "")

    it "parses <=" $ do
      runParser parseMLLRelOp "<=" `shouldBe` Right (Le, "")

  describe "parseMLLOp" $ do
    it "parses +" $ do
      runParser parseMLLOp "+" `shouldBe` Right (Add, "")

    it "parses -" $ do
      runParser parseMLLOp "-" `shouldBe` Right (Sub, "")

    it "parses *" $ do
      runParser parseMLLOp "*" `shouldBe` Right (Mul, "")

    it "parses /" $ do
      runParser parseMLLOp "/" `shouldBe` Right (Div, "")

  describe "parseMLLInlineComment" $ do
    it "parses inline comments" $ do
      runParser parseMLLInlineComment "// This is a comment\n" 
        `shouldBe` Right (" This is a comment", "")

  describe "parseMLLMultiLineComment" $ do
    it "parses multi-line comments" $ do
      runParser parseMLLMultiLineComment "/* This is a \nmulti-line comment */" 
        `shouldBe` Right (" This is a \nmulti-line comment ", "")
