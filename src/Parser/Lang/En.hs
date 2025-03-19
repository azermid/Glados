module Parser.Lang.En where

import Control.Applicative
import Parser.Lib

parseMLLBoolEnFalse :: Parser Bool
parseMLLBoolEnFalse = parseString "False" *> pure False

parseMLLBoolEnTrue :: Parser Bool
parseMLLBoolEnTrue = parseString "True" *> pure True

parseMLLBoolEn :: Parser Bool
parseMLLBoolEn = parseMLLBoolEnFalse <|> parseMLLBoolEnTrue

parseMLLReturnEn :: Parser String
parseMLLReturnEn = parseUselessChar *> parseString "return"

parseMLLIfEn :: Parser String
parseMLLIfEn = parseUselessChar *> parseString "if"

parseMLLElseEn :: Parser String
parseMLLElseEn = parseUselessChar *> parseString "else"

parseMLLWhileEn :: Parser String
parseMLLWhileEn = parseUselessChar *> parseString "while"

parseMLLForEn :: Parser String
parseMLLForEn = parseUselessChar *> parseString "for"

parseMLLImportEn :: Parser String
parseMLLImportEn = parseUselessChar *> parseString "include"