module Parser.Lang.Fr where

import Control.Applicative
import Parser.Lib

parseMLLBoolFrFaux :: Parser Bool
parseMLLBoolFrFaux = parseString "Faux" *> pure False

parseMLLBoolFrVrai :: Parser Bool
parseMLLBoolFrVrai = parseString "Vrai" *> pure True

parseMLLBoolFr :: Parser Bool
parseMLLBoolFr = parseMLLBoolFrFaux <|> parseMLLBoolFrVrai

parseMLLReturnFr :: Parser String
parseMLLReturnFr = parseUselessChar *> (parseString "retourne" <|> parseString "retour" )

parseMLLIfFr :: Parser String
parseMLLIfFr = parseUselessChar *> parseString "si"

parseMLLElseFr :: Parser String
parseMLLElseFr = parseUselessChar *> parseString "sinon"

parseMLLWhileFr :: Parser String
parseMLLWhileFr = parseUselessChar *> parseString "tant" <* parseUselessChar *> parseString "que"

parseMLLForFr :: Parser String
parseMLLForFr = parseUselessChar *> parseString "pour"

parseMLLImportFr :: Parser String
parseMLLImportFr = parseUselessChar *> parseString "importer"