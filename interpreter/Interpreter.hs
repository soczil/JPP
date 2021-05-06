module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Lattepp.Abs
import Data.Map as M

type Var = String 
type Loc = Int
type Env = M.Map Var Loc
type LPPEnv = M.Map Ident Loc
type LPPStore = M.Map Loc Value
type LPPState = (LPPStore, LPPEnv, Loc, Bool)
type LPPExcept = ExceptT LPPError IO
type LPPReader = ReaderT Env LPPExcept
type LPPMonad = StateT LPPState LPPReader

data Value = VInt Integer | VString String | VBool Bool
    deriving (Ord, Eq)

data LPPError =  DivisionByZero | ModuloByZero | VarAlreadyDeclared | VarNotDeclared | UnknownError deriving Show

emptyState :: LPPState
emptyState = (M.empty, M.empty, 0, False) 

newEnv :: Env
newEnv = M.empty

valueToString :: Value -> String
valueToString (VInt x) = show x
valueToString (VString s) = s
valueToString (VBool x) = show x

defaultValue :: Type -> Value
defaultValue Int = VInt 0
defaultValue Bool = VBool False
defaultValue Str = VString ""

varToEnv :: Ident -> LPPMonad ()
varToEnv id = do
    (store, env, loc, doElse) <- get
    case M.lookup id env of
        Just _ -> throwError VarAlreadyDeclared
        Nothing -> do
            let updatedEnv = M.insert id loc env
            put (store, updatedEnv, loc + 1, doElse)

getVarLoc :: Ident -> LPPMonad Loc
getVarLoc id = do
    (_, env, _, _) <- get
    case M.lookup id env of
        Just loc -> return loc
        Nothing -> throwError VarNotDeclared

getVarValue :: Ident -> LPPMonad Value
getVarValue id = do
    loc <- getVarLoc id
    (store, _, _, _) <- get
    case M.lookup loc store of
        Just val -> return val
        Nothing -> throwError UnknownError

updateVarValue :: Ident -> Value -> LPPMonad ()
updateVarValue id val = do
    varLoc <- getVarLoc id
    (store, env, loc, doElse) <- get
    let updatedStore = M.insert varLoc val store
    put (updatedStore, env, loc, doElse)

execItem :: Type -> Item -> LPPMonad ()
execItem t (NoInit id) = do
    varToEnv id
    updateVarValue id $ defaultValue t
execItem t (Init id e) = do
    val <- evalExpr e
    varToEnv id
    updateVarValue id val 

execBlock :: Block -> LPPMonad ()
execBlock (Block []) = return()
execBlock (Block stmts) = mapM_ execStmt stmts

getElseFlag :: LPPMonad Bool
getElseFlag = do
    (_, _, _, doElse) <- get 
    return doElse

setElseFlag :: Bool -> LPPMonad ()
setElseFlag val = do
    (store, env, loc, _) <- get
    put (store, env, loc, val)

execElif :: Elif -> LPPMonad ()
execElif (Elif e block) = do
    doElse <- getElseFlag
    VBool val <- evalExpr e
    when (doElse && val) $ do
        execBlock block
        setElseFlag False

execStmt :: Stmt -> LPPMonad ()
execStmt (BStmt block) = execBlock block
execStmt Empty = return ()
execStmt (DStmt decl) =
    case decl of
        (NormalDecl t itms) -> mapM_ (execItem t) itms
        (FinalDecl t itms) -> undefined 
execStmt (Ass id e) = do
    val <- evalExpr e
    updateVarValue id val
execStmt (Inc id) = do
    VInt val <- getVarValue id
    updateVarValue id $ VInt (val + 1)
execStmt (Dec id) = do
    VInt val <- getVarValue id
    updateVarValue id $ VInt (val - 1)
execStmt (Cond e block elifs) = do
    VBool val <- evalExpr e
    if val 
        then execBlock block
        else do
            oldFlag <- getElseFlag
            setElseFlag True
            mapM_ execElif elifs
            setElseFlag oldFlag
execStmt (CondElse e block1 elifs block2) = do
    VBool val <- evalExpr e
    if val
        then execBlock block1
        else do
            oldFlag <- getElseFlag
            setElseFlag True
            mapM_ execElif elifs
            doElse <- getElseFlag
            when doElse $ execBlock block2
            setElseFlag oldFlag
execStmt (While e block) = do
    VBool val <- evalExpr e
    when val $ do
        execBlock block
        execStmt (While e block)
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
evalExpr (Evar id) = getVarValue id 
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EString s) = return $ VString s
evalExpr (Neg e) = do
    VInt val <- evalExpr e
    return $ VInt (-val)
evalExpr (Not e) = do
    VBool val <- evalExpr e
    return $ VBool (not val)
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
evalExpr (EAnd e1 e2) = do
    VBool val1 <- evalExpr e1
    VBool val2 <- evalExpr e2
    return $ VBool (val1 && val2)
evalExpr (EOr e1 e2) = do
    VBool val1 <- evalExpr e1
    VBool val2 <- evalExpr e2
    return $ VBool (val1 || val2)

execFun :: FunDef -> LPPMonad ()
execFun (FunDef _ _ _ block) = execBlock block

interpret :: Program -> IO (String, Int)
interpret (Program fundefs) = do
    let runS = runStateT (execFun $ head fundefs) emptyState
    let runR = runReaderT runS newEnv
    result <- runExceptT runR
    case result of
        Left err -> do
            return ("Error: " ++ show err, 1)
        Right msg -> do
            return ("NoError", 0)
