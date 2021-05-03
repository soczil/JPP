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
    deriving (Eq, Show)

newStore :: LPPStore
newStore = M.empty 

newEnv :: Env
newEnv = M.empty

valueToString :: Value -> String
valueToString (VInt x) = show x
valueToString (VString s) = s

execStmt :: Stmt -> LPPMonad ()
execStmt Empty = return ()
execStmt (Print e) = do
    val <- evalExpr e
    liftIO $ putStrLn $ valueToString val

evalExpr :: Expr -> LPPMonad Value
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EString s) = return $ VString s

-- jol :: Expr -> LPPMonad String
-- jol e = do
--     val <- evalExpr e
--     return $ valueToString val

-- jol2 :: Expr -> LPPMonad ()
-- jol2 e = do
--     val <- evalExpr e
--     liftIO $ print val

execBlock :: Block -> LPPMonad ()
execBlock (Block (x:xs)) = execStmt x

execFun :: FunDef -> LPPMonad ()
execFun (FunDef t ident [] block) = execBlock block

interpret :: Program -> IO ()
interpret (Program fundefs) = do
    void $ runReaderT (runStateT (execFun $ head fundefs) newStore) newEnv
