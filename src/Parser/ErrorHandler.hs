-- 
-- EPITECH PROJECT, 2024
-- B-FUN-500-MPL-5-2-glados-edgar.maurel
-- File description:
-- ErrorHandler.hs
--

module Parser.ErrorHandler where

import Parser.Lib

-- | ParserError
getIndex :: String -> String -> Int
getIndex str1 str2 = length str1 - length str2

-- | setIndicator
setIndicator :: Int -> Int -> String
setIndicator index len = replicate index ' ' ++ replicate len '^'

-- | getLineAndColumn
getLineAndColumn :: String -> Int -> (Int, Int)
getLineAndColumn input index = go (lines input) index 1 0
  where
    go [] _ line _ = (line, 1)
    go (l:ls) i line acc
      | i < length l + 1 = (line, i + 1)
      | otherwise         = go ls (i - length l - 1) (line + 1) (acc + length l + 1)

-- | getLineFromLineNumber
getLineFromLineNumber :: String -> Int -> String
getLineFromLineNumber input lineNumber =
  case drop (lineNumber - 1) (lines input) of
    (l:_) -> l
    []    -> ""

-- | showError
showError :: ParserError -> String -> String
showError (Missing (e, rest, cause)) str = err ++ whereErr ++ line ++ rest
  where
    index               = getIndex str rest
    (lineAt, columnAt)  = getLineAndColumn str index
    err                 = "Error: Missing " ++ e ++ " at line "
    whereErr            = show lineAt ++ " and column " ++ show columnAt ++ 
                          " (" ++ cause ++ ")" ++ "\n"
    line                = getLineFromLineNumber str lineAt ++ "\n" ++ 
                          setIndicator columnAt (length e) ++ "\nleft with: "
