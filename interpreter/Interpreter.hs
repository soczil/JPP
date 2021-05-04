module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Lattepp.Abs
import Data.Map as M

type Var = String 
type Loc = Int
type Env = M.Map Var Loc
type LPPStore = M.Map Loc Expr
type LPPReader = ReaderT Env IO
type LPPMonad = StateT LPPStore LPPReader

data Value = VInt Integer | VString String | VBool Bool

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

evalOp :: (Integer -> Integer -> Integer) -> Integer -> Integer -> LPPMonad Value
evalOp (+) val1 val2 = return $ VInt (val1 + val2)

evalExpr :: Expr -> LPPMonad Value
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EString s) = return $ VString s
evalExpr (EAdd e1 addOp e2) = do
    VInt val1 <- evalExpr e1
    VInt val2 <- evalExpr e2
    evalOp (+) val1 val2


execBlock :: Block -> LPPMonad ()
execBlock (Block []) = return()
execBlock (Block stmts) = mapM_ execStmt stmts

execFun :: FunDef -> LPPMonad ()
execFun (FunDef t ident [] block) = execBlock block

interpret :: Program -> IO ()
interpret (Program fundefs) = do
    void $ runReaderT (runStateT (execFun $ head fundefs) newStore) newEnv
