module Typechecker where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import qualified Data.Map as M
import qualified Data.Set as S

type TCVarInf = (Type, Bool)
type TCEnv = M.Map Ident TCVarInf
type TCOldIds = S.Set Ident
type TCFunInf = (Type, [(Ident, Type)])
type TCFunEnv = M.Map Ident TCFunInf
type TCExcept = ExceptT TCError IO
type TCState = (TCEnv, TCFunEnv)
type TCMonad = StateT TCState TCExcept

data TCError = VarAlreadyDeclared
             | VarNotDeclared Ident
             | FunNotDeclared
             | FunAlreadyDeclared
             | WrongType
             | MainFunError
             | FinalVarAssignment
    deriving Show

-- do zmiany
emptyState :: TCState
emptyState = (M.empty, M.empty)

varToEnv :: Ident -> Type -> Bool -> TCMonad ()
varToEnv id t final = do
    (env, funEnv) <- get
    case M.lookup id env of
        Nothing -> put (M.insert id (t, final) env, funEnv)
        _ -> throwError VarAlreadyDeclared

getVarInf :: Ident -> TCMonad TCVarInf
getVarInf id = do
    (env, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id
        Just inf -> return inf

getVarType :: Ident -> TCMonad Type
getVarType id = do
    (t, _) <- getVarInf id
    return t

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit id) = varToEnv id t final
checkItem t final (Init id e) = do
    checkExprType e t
    varToEnv id t final

checkDecl :: Decl -> TCMonad ()
checkDecl (NormalDecl t itms) = mapM_ (checkItem t False) itms
checkDecl (FinalDecl t itms) = mapM_ (checkItem t True) itms

-- Pierwsza wersja !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
checkBlock :: Block -> TCMonad ()
checkBlock (Block stmts) = mapM_ checkStmt stmts

checkFinal :: Bool -> TCMonad ()
checkFinal final = when final $ throwError FinalVarAssignment

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt block) = checkBlock block
checkStmt Empty = return ()
checkStmt (FStmt fundef) = undefined
checkStmt (ArrDecl t itms) = undefined
checkStmt (ArrAss id e1 e2) = undefined
checkStmt (DStmt decl) = checkDecl decl
checkStmt (Ass id e) = do
    (t, final) <- getVarInf id
    checkFinal final
    checkExprType e t
checkStmt (Inc id) = do
    (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Dec id) = do
    (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Ret e) = undefined
checkStmt RetV = undefined
checkStmt Break = undefined
checkStmt Continue = undefined
checkStmt (Cond e block elifs) = do
    checkExprType e Bool
    checkBlock block -- poprawic

checkType :: Type -> Type -> TCMonad ()
checkType expected actual = when (expected /= actual) $ throwError WrongType

checkExprType :: Expr -> Type -> TCMonad ()
checkExprType e t = do
    actualType <- checkExpr e
    checkType t actualType

checkOpType :: Expr -> Expr -> Type -> TCMonad Type
checkOpType e1 e2 t = do
    checkExprType e1 t
    checkExprType e2 t
    return t

checkPostfixOpType :: Ident -> TCMonad Type
checkPostfixOpType id = do
    t <- getVarType id
    checkType Int t
    return Int

checkPrefixOpType :: Expr -> Type -> TCMonad Type
checkPrefixOpType e t = do
    checkExprType e t
    return t

checkExpr :: Expr -> TCMonad Type
checkExpr (Evar id) = getVarType id
checkExpr (ELitInt _) = return Int
checkExpr ELitTrue = return Bool
checkExpr ELitFalse = return Bool
checkExpr (ArrRead id e) = do
    checkExprType e Int
    getVarType id
checkExpr (EString _) = return Str
checkExpr (Neg e) = checkPrefixOpType e Int
checkExpr (Not e) = checkPrefixOpType e Bool
checkExpr (EMul e1 _ e2) = checkOpType e1 e2 Int
checkExpr (EAdd e1 _ e2) = checkOpType e1 e2 Int
checkExpr (EInc id) = checkPostfixOpType id
checkExpr (EDec id) = checkPostfixOpType id
checkExpr (ERel e1 _ e2) = undefined
checkExpr (EAnd e1 e2) = checkOpType e1 e2 Bool
checkExpr (EOr e1 e2) = checkOpType e1 e2 Bool

funToEnv :: FunDef -> TCMonad ()
funToEnv (FunDef t id args _) = do
    (env, funEnv) <- get
    case M.lookup id funEnv of
        Nothing -> do
            let argsList = foldr (\(Arg t id) acc -> (id, t) : acc) [] args
            put (env, M.insert id (t, argsList) funEnv)
        _ -> throwError FunAlreadyDeclared

argsToList :: [Arg] -> [(Ident, Type)]
argsToList = foldr (\(Arg t id) acc -> (id, t) : acc) []

getFunInf :: Ident -> TCMonad TCFunInf
getFunInf id = do
    (_, funEnv) <- get
    case M.lookup id funEnv of
        Nothing -> throwError FunNotDeclared
        Just inf -> return inf 

checkFun :: FunDef -> TCMonad ()
checkFun (FunDef t id _ block) = do
    checkBlock block -- just for now

checkMain :: TCMonad ()
checkMain = do
    (t, args) <- getFunInf $ Ident "main"
    when (t /= Int) $ throwError MainFunError
    when (args /= []) $ throwError MainFunError

checkEveryFun :: [FunDef] -> TCMonad ()
checkEveryFun fundefs = do
    mapM_ funToEnv fundefs
    checkMain
    mapM_ checkFun fundefs

check :: Program -> IO (String, Bool)
check (Program fundefs) = do
    let runS = runStateT (checkEveryFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("Error: " ++ show err, True)
        Right _ -> return ("Typecheck finished without errors", False)
