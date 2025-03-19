module Main (main) where
import VM.Runtime

import Args

import Runner

import System.Exit (exitWith, ExitCode(..))


main :: IO Int
main = do
    opts <- getOption
    case opts of
        Options { optHelp = True } -> do
            putStrLn showUsage
            return 0

        Options { optExec = True } -> do
            res <- runOptExec opts
            exitWith (exitCode res)

        Options { optBuild = True } -> runOptBuild opts

        Options { optLoad = Just _ } -> do
            res <- runOptLoad opts
            exitWith (exitCode res)

        Options { optParse = Just _ } -> do
            res <- runOptParse opts
            exitWith (exitCode res)

        Options { optDecompile = Just _ } -> do
            res <- runOptDecompile opts
            exitWith (exitCode res)

        Options { optInterpret = True } -> do
            res <- display
            exitWith (exitCode res)

        _ -> do
            putStrLn showUsage
            return 0
  where
    exitCode res
        | res == 0  = ExitSuccess
        | otherwise = ExitFailure res

