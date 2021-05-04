module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Lattepp.Abs
import Data.Map as M

type Var = String 
type Loc = Int
type Env = M.Map Var Loc
type LPPStore = M.Map Loc Expr
type LPPExcept = ExceptT LPPError IO
type LPPReader = ReaderT Env LPPExcept
type LPPMonad = StateT LPPStore LPPReader

data Value = VInt Integer | VString String | VBool Bool
    deriving (Ord, Eq)

data LPPError =  DivisionByZero | ModuloByZero deriving Show

newStore :: LPPStore
newStore = M.empty 

newEnv :: Env
newEnv = M.empty

valueToString :: Value -> String
valueToString (VInt x) = show x
valueToString (VString s) = s
valueToString (VBool x) = show x

execStmt :: Stmt -> LPPMonad ()
execStmt Empty = return ()
execStmt (Print e) = do
    val <- evalExpr e
    liftIO $ putStrLn $ valueToString val
execStmt (Ret e) = do
    val <- evalExpr e
    liftIO $ putStrLn ("jol " ++ valueToString val)

evalAddOp :: AddOp -> Integer -> Integer -> LPPMonad Value
evalAddOp Plus val1 val2 = return $ VInt (val1 + val2)
evalAddOp Minus val1 val2 = return $ VInt (val1 - val2)

evalMulOp :: MulOp -> Integer -> Integer -> LPPMonad Value
evalMulOp Times val1 val2 = return $ VInt (val1 * val2)
evalMulOp Div val1 val2 = do
    if val2 == 0 
        then throwError DivisionByZero 
        else return $ VInt (val1 `div` val2)
evalMulOp Mod val1 val2 = do
    if val2 == 0
        then throwError ModuloByZero
        else return $ VInt (val1 `mod` val2)

evalRelOp :: RelOp -> Value -> Value -> LPPMonad Value
evalRelOp Lt val1 val2 = return $ VBool (val1 < val2)
evalRelOp Leq val1 val2 = return $ VBool (val1 <= val2)
evalRelOp Gt val1 val2 = return $ VBool (val1 > val2)
evalRelOp Geq val1 val2 = return $ VBool (val1 >= val2)
evalRelOp Eq val1 val2 = return $ VBool (val1 == val2)
evalRelOp Neq val1 val2 = return $ VBool (val1 /= val2)

evalExpr :: Expr -> LPPMonad Value
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EString s) = return $ VString s
evalExpr (EAdd e1 addOp e2) = do
    VInt val1 <- evalExpr e1
    VInt val2 <- evalExpr e2
    evalAddOp addOp val1 val2
evalExpr (EMul e1 mulOp e2) = do
    VInt val1 <- evalExpr e1
    VInt val2 <- evalExpr e2
    evalMulOp mulOp val1 val2
evalExpr (ERel e1 relOp e2) = do
    val1 <- evalExpr e1
    val2 <- evalExpr e2
    evalRelOp relOp val1 val2

execBlock :: Block -> LPPMonad ()
execBlock (Block []) = return()
execBlock (Block stmts) = mapM_ execStmt stmts

execFun :: FunDef -> LPPMonad ()
execFun (FunDef t ident [] block) = execBlock block

interpret :: Program -> IO (String, Int)
interpret (Program fundefs) = do
    let runS = runStateT (execFun $ head fundefs) newStore
    let runR = runReaderT runS newEnv
    result <- runExceptT runR
    case result of
        Left err -> do
            return ("Error: " ++ show err, 1)
        Right msg -> do
            return ("NoError", 0)
