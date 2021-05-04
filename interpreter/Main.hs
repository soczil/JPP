module Main where

import System.Environment
import System.Exit

import Interpreter

import Lattepp.Par
import Lattepp.ErrM

exitInterpreter :: (String, Int) -> IO ()
exitInterpreter (msg, 0) = return ()
exitInterpreter (err, num) = do
    putStrLn err
    exitWith $ ExitFailure num

parseAndInterpret :: String -> IO ()
parseAndInterpret lppProgram = do
    case pProgram (myLexer lppProgram) of
        Ok prog -> do
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
