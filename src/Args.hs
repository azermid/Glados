module Args (Options(..), defaultOptions, getOption, showUsage) where

import System.Environment (getArgs)
import System.Console.GetOpt
import Data.Maybe (fromMaybe)

{-# LANGUAGE DeriveDataTypeable #-}

-- possible option types
data Options = Options
    { optExec :: Bool
    , optBuild :: Bool
    , optOutput :: Maybe String
    , optSource :: Maybe [String]
    , optLoad :: Maybe String
    , optHelp :: Bool
    , optParse :: Maybe String
    , optDebug :: Bool
    , optDecompile :: Maybe String
    , optInterpret :: Bool
    , optVerbose :: Bool
    } deriving Show

-- default options
defaultOptions :: Options
defaultOptions = Options { optExec = False
                        , optHelp = False
                        , optBuild = False
                        , optOutput = Nothing
                        , optSource = Nothing
                        , optLoad = Nothing
                        , optParse = Nothing
                        , optDebug = False
                        , optDecompile = Nothing
                        , optInterpret = False
                        , optVerbose = False
                        }

-- option descriptions and necessary actions
options :: [OptDescr (Options -> Options)]
options =
    [ Option ['e'] ["exec"]
        (NoArg (\opts -> opts { optExec = True }))
        "Execute a file"
    , Option ['b'] ["build"]
        (NoArg (\opts -> opts { optBuild = True }))
        "Build a file"
    , Option ['o'] ["output"]
        (ReqArg (\f opts -> opts { optOutput = Just f }) "FILE")
        "Output name of the build file"
    , Option ['s'] ["source"]
        (ReqArg (\f opts -> opts { optSource = Just (fromMaybe [] (optSource opts) ++ [f]) }) "FILE")
        "Source file"
    , Option ['l'] ["load"]
        (ReqArg (\f opts -> opts { optLoad = Just f }) "FILE")
        "Load a file for execution"
    , Option ['h'] ["help"]
        (NoArg (\opts -> opts { optHelp = True }))
        "Show help"
    , Option ['p'] ["parse"]
        (ReqArg (\f opts -> opts { optParse = Just f }) "FILE")
        "Parse a file"
    , Option ['d'] ["decompile"]
        (ReqArg (\f opts -> opts { optDecompile = Just f }) "FILE")
        "Decompile a file"
    , Option ['i'] ["interpret"]
        (NoArg (\opts -> opts { optInterpret = True }))
        "Run the interpreter"
    , Option ['v'] ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "Verbose mode"
    ]

-- process the command line arguments
getOption :: IO Options
getOption = do
    args <- getArgs
    let (actions, nonOptions, errors) = getOpt Permute options args
    if not (null errors)
        then ioError (userError (concat errors ++ usageInfo header options))
        else do
            let opts = foldl (flip id) defaultOptions actions
            return $ case nonOptions of
                [] -> opts
                (_:_) -> opts { optSource = Just (fromMaybe [] (optSource opts)
                ++ nonOptions) }
  where
    header = "Usage: prog [OPTION...] files..."

-- show the usage information
showUsage :: String
showUsage = usageInfo "Usage: glados [OPTION...]" options
