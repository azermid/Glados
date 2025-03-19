module Parser.Lang.Pt where

import Control.Applicative
import Parser.Lib

parseMLLBoolPtFalse :: Parser Bool
parseMLLBoolPtFalse = parseString "Falso" *> pure False

parseMLLBoolPtTrue :: Parser Bool
parseMLLBoolPtTrue = parseString "Verdadeiro" *> pure True

parseMLLBoolPt :: Parser Bool
parseMLLBoolPt = parseMLLBoolPtFalse <|> parseMLLBoolPtTrue

parseMLLReturnPt :: Parser String
parseMLLReturnPt = parseUselessChar *> parseString "retornar"

parseMLLIfPt :: Parser String
parseMLLIfPt = parseUselessChar *> parseString "se"

parseMLLElsePt :: Parser String
parseMLLElsePt = parseUselessChar *> parseString "senão" <|> parseUselessChar *> parseString "senao"

parseMLLElseIfPt :: Parser String
parseMLLElseIfPt = parseUselessChar *> parseString "senão se"

parseMLLWhilePt :: Parser String
parseMLLWhilePt = parseUselessChar *> parseString "enquanto"

parseMLLForPt :: Parser String
parseMLLForPt = parseUselessChar *> parseString "para"

parseMLLImportPt :: Parser String
parseMLLImportPt = parseUselessChar *> parseString "importar"
