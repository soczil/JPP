module Main where

import System.IO (hPutStrLn, stderr)
import System.Environment (getArgs)
import System.Exit (ExitCode(ExitFailure), exitWith)

import Lattepp.Par
import Lattepp.ErrM

import Interpreter
import Typechecker

exitInterpreter :: (String, Bool) -> IO ()
exitInterpreter (msg, False) = return ()
exitInterpreter (err, True) = do
    hPutStrLn stderr err
    exitWith $ ExitFailure 1

finishTypechecker :: (String, Bool) -> IO ()
finishTypechecker (msg, False) = return ()
finishTypechecker (err, True) = do
    hPutStrLn stderr err
    exitWith $ ExitFailure 2

parseAndInterpret :: String -> IO ()
parseAndInterpret lppProgram = do
    case pProgram (myLexer lppProgram) of
        Ok prog -> do
            result <- check prog
            finishTypechecker result
            result <- interpret prog
            exitInterpreter result
        Bad msg -> putStrLn msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Podaj plik z programem interpretacji"
        (x:_) -> do
            lppProgram <- readFile x
            parseAndInterpret lppProgram
