module Typechecker where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import qualified Data.Map as M
import qualified Data.Set as S

type TCVarInf = (Type, Bool)
type TCEnv = M.Map Ident TCInf
type TCOldVars = S.Set Ident
type TCFunInf = (Type, [(Ident, Type)])
type TCFunEnv = M.Map Ident TCInf
type TCExcept = ExceptT TCError IO
type TCState = (TCEnv, TCFunEnv, TCOldVars)
type TCMonad = StateT TCState TCExcept

data TCInf = VarInf (Type, Bool) | FunInf (Type, [(Ident, Type)])

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
emptyState = (M.empty, M.empty, S.empty)

varToEnv :: Ident -> Type -> Bool -> TCMonad ()
varToEnv id t final = do
    (env, funEnv, oldVars) <- get
    when (M.member id env && S.notMember id oldVars) $ throwError VarAlreadyDeclared 
    put (M.insert id (VarInf (t, final)) env, funEnv, S.delete id oldVars)

getVarInf :: Ident -> TCMonad TCInf
getVarInf id = do
    (env, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id
        Just inf -> return inf

getVarType :: Ident -> TCMonad Type
getVarType id = do
    VarInf (t, _) <- getVarInf id
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

setNewOldVars :: TCMonad ()
setNewOldVars = do
    (env, funEnv, _) <- get
    let oldVars = S.fromAscList $ M.keys env
    put (env, funEnv, oldVars)

checkBlockNewEnv :: Block -> TCMonad ()
checkBlockNewEnv block = do
    (oldEnv, oldFunEnv, oldOldVars) <- get
    setNewOldVars
    checkBlock block
    put (oldEnv, oldFunEnv, oldOldVars)

checkFinal :: Bool -> TCMonad ()
checkFinal final = when final $ throwError FinalVarAssignment

checkElif :: Elif -> TCMonad ()
checkElif (Elif e block) = do
    checkExprType e Bool
    checkBlockNewEnv block

checkForInit :: ForInit -> TCMonad ()
checkForInit (ForInitExpr exprs) = mapM_ checkExpr exprs
checkForInit (ForInitVar decl) = checkStmt (DStmt decl)

checkArrItem :: Type -> ArrItem -> TCMonad ()
checkArrItem t (ArrNoInit id) = varToEnv id (Array t) False
checkArrItem t (ArrInit id e1 e2) = do
    checkExprType e1 Int
    checkExprType e2 t
    varToEnv id (Array t) False

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt block) = checkBlock block
checkStmt Empty = return ()
checkStmt (FStmt fundef) = undefined
checkStmt (ArrDecl t itms) = mapM_ (checkArrItem t) itms 
checkStmt (ArrAss id e1 e2) = do
    checkExprType e1 Int
    arrType <- getVarType id
    exprType <- checkExpr e2
    checkType arrType (Array exprType)
checkStmt (DStmt decl) = checkDecl decl
checkStmt (Ass id e) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkExprType e t
checkStmt (Inc id) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Dec id) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Ret e) = undefined
checkStmt RetV = undefined
checkStmt Break = undefined
checkStmt Continue = undefined
checkStmt (Cond e block elifs) = do
    checkExprType e Bool
    checkBlockNewEnv block
    mapM_ checkElif elifs
checkStmt (CondElse e block1 elifs block2) = do
    checkExprType e Bool
    checkBlockNewEnv block1
    mapM_ checkElif elifs
    checkBlockNewEnv block2
checkStmt (While e block) = do
    checkExprType e Bool
    checkBlockNewEnv block
checkStmt (For init e exprs block) = do
    (oldEnv, oldFunEnv, oldOldVars) <- get
    setNewOldVars
    checkForInit init
    checkExprType e Bool
    mapM_ checkExpr exprs
    put (oldEnv, oldFunEnv, oldOldVars)
checkStmt (ForIn id1 id2 block) = undefined
checkStmt (EStmt e) = void $ checkExpr e
checkStmt (Print e) = void $ checkExpr e


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
checkExpr (ERel e1 _ e2) = do
    t <- checkExpr e1
    checkExprType e2 t
    return Bool
checkExpr (EAnd e1 e2) = checkOpType e1 e2 Bool
checkExpr (EOr e1 e2) = checkOpType e1 e2 Bool

funToEnv :: FunDef -> TCMonad ()
funToEnv (FunDef t id args _) = do
    (env, funEnv, oldVars) <- get
    case M.lookup id funEnv of
        Nothing -> do
            let argsList = foldr (\(Arg t id) acc -> (id, t) : acc) [] args
            put (env, M.insert id (FunInf (t, argsList)) funEnv, oldVars)
        _ -> throwError FunAlreadyDeclared

argsToList :: [Arg] -> [(Ident, Type)]
argsToList = foldr (\(Arg t id) acc -> (id, t) : acc) []

getFunInf :: Ident -> TCMonad TCInf
getFunInf id = do
    (_, funEnv, _) <- get
    case M.lookup id funEnv of
        Nothing -> throwError FunNotDeclared
        Just inf -> return inf 

checkTopFun :: FunDef -> TCMonad ()
checkTopFun (FunDef t _ _ block) = do
    (_, funEnv, _) <- get
    put (funEnv, funEnv, S.empty)
    checkBlock block

checkMain :: TCMonad ()
checkMain = do
    FunInf (t, args) <- getFunInf $ Ident "main"
    when (t /= Int) $ throwError MainFunError
    when (args /= []) $ throwError MainFunError

checkEveryFun :: [FunDef] -> TCMonad ()
checkEveryFun fundefs = do
    mapM_ funToEnv fundefs
    checkMain
    mapM_ checkTopFun fundefs

check :: Program -> IO (String, Bool)
check (Program fundefs) = do
    let runS = runStateT (checkEveryFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("Error: " ++ show err, True)
        Right _ -> return ("Typecheck finished without errors", False)
