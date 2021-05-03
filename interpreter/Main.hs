module Main where

import System.Environment

import Interpreter

import Lattepp.Par
import Lattepp.ErrM

parseProgram :: String -> IO ()
parseProgram lppProgram = do
    case pProgram (myLexer lppProgram) of
        Ok prog -> interpret prog
        Bad msg -> putStr msg

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Podaj plik z programem interpretacji"
        (x:_) -> do
            putStrLn ("Interpretacja programu " ++ x)
            lppProgram <- readFile x
            parseProgram lppProgram
