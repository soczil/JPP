module Typechecker where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import qualified Data.Map as M
import qualified Data.Set as S

type TCEnv = M.Map Ident TCInf
type TCOldVars = S.Set Ident
type TCExcept = ExceptT TCError IO
type TCState = (TCEnv, TCOldVars, Type)
type TCMonad = StateT TCState TCExcept

data TCInf = VarInf (Type, Bool) | FunInf (Type, [Type])
    deriving Eq

data TCError = VarAlreadyDeclared
             | VarNotDeclared Ident
             | FunNotDeclared
             | FunAlreadyDeclared
             | WrongType
             | MainFunError
             | FinalVarAssignment
             | ReturnTypeError
             | WrongArgumentType
             | WrongNumberOfArguments
             | NotAnArray
             | WTF
    deriving Show

-- do zmiany
emptyState :: TCState
emptyState = (M.empty, S.empty, Void)

varToEnv :: Ident -> Type -> Bool -> TCMonad ()
varToEnv id t final = do
    (env, oldVars, retType) <- get
    when (M.member id env && S.notMember id oldVars) $ throwError VarAlreadyDeclared 
    put (M.insert id (VarInf (t, final)) env, S.delete id oldVars, retType)

getVarInf :: Ident -> TCMonad TCInf
getVarInf id = do
    (env, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id
        Just inf -> return inf

getVarType :: Ident -> TCMonad Type
getVarType id = do
    inf <- getVarInf id
    case inf of
        VarInf (t, _) -> return t
        FunInf (t, _) -> return t

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit id) = varToEnv id t final
checkItem t final (Init id e) = do
    checkExprType e t
    varToEnv id t final

checkDecl :: Decl -> TCMonad ()
checkDecl (NormalDecl t itms) = mapM_ (checkItem t False) itms
checkDecl (FinalDecl t itms) = mapM_ (checkItem t True) itms

checkBlock :: Block -> TCMonad ()
checkBlock (Block stmts) = mapM_ checkStmt stmts

setNewOldVars :: TCMonad ()
setNewOldVars = do
    (env, _, retType) <- get
    let oldVars = S.fromAscList $ M.keys env
    put (env, oldVars, retType)

checkBlockNewEnv :: Block -> TCMonad ()
checkBlockNewEnv block = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)

checkFinal :: Bool -> TCMonad ()
checkFinal final = when final $ throwError FinalVarAssignment

checkElif :: Elif -> TCMonad ()
checkElif (Elif e block) = do
    checkExprType e Bool
    checkBlockNewEnv block

getItemIdent :: Item -> Ident
getItemIdent (NoInit id) = id
getItemIdent (Init id _) = id

checkForInit :: ForInit -> TCMonad [Ident]
checkForInit (ForInitExpr exprs) = do
    mapM_ checkExpr exprs
    return []
checkForInit (ForInitVar decl) = do
    checkStmt (DStmt decl)
    case decl of
        (NormalDecl _ _) -> return []
        (FinalDecl _ itms) -> return $ map getItemIdent itms

checkArrItem :: Type -> ArrItem -> TCMonad ()
checkArrItem t (ArrNoInit id) = varToEnv id (Array t) False
checkArrItem t (ArrInit id e1 e2) = do
    checkExprType e1 Int
    checkExprType e2 t
    varToEnv id (Array t) False

checkReturn :: Type -> TCMonad ()
checkReturn t = do
    (_, _, retType) <- get
    when (t /= retType) $ throwError ReturnTypeError

funArgsToEnv :: [Arg] -> TCMonad ()
funArgsToEnv = mapM_ (\(Arg argType argId) -> varToEnv argId argType False)

setRetType :: Type -> TCMonad ()
setRetType t = do
    (env, oldVars, _) <- get
    put (env, oldVars, t)

checkFun :: FunDef -> TCMonad ()
checkFun (FunDef t id args block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    funArgsToEnv args
    setRetType t
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)

updateVarFinal :: Bool -> Ident -> TCMonad ()
updateVarFinal newFinal id = do
    VarInf (t, final) <- getVarInf id
    (env, oldVars, retType) <- get
    put (M.insert id (VarInf (t, newFinal)) env, oldVars, retType)

checkIfArray :: Type -> Bool
checkIfArray (Array _) = True
checkIfArray _ = False

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt block) = checkBlock block
checkStmt Empty = return ()
checkStmt (FStmt fundef) = do
    funToEnv fundef
    checkFun fundef
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
checkStmt (Ret e) = do
    t <- checkExpr e
    checkReturn t
checkStmt RetV = checkReturn Void
checkStmt Break = return ()
checkStmt Continue = return ()
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
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    finalVars <- checkForInit init
    checkExprType e Bool
    mapM_ (updateVarFinal False) finalVars
    mapM_ checkExpr exprs
    mapM_ (updateVarFinal True) finalVars
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
checkStmt (ForIn id1 id2 block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    t <- getVarType id2
    unless (checkIfArray t) $ throwError NotAnArray
    let (Array id1Type) = t
    varToEnv id1 id1Type True
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
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
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
    return Int

checkPrefixOpType :: Expr -> Type -> TCMonad Type
checkPrefixOpType e t = do
    checkExprType e t
    return t

checkFunArg :: (Type, Expr) -> TCMonad ()
checkFunArg (t, e) = do
    exprType <- checkExpr e
    when (t /= exprType) $ throwError WrongArgumentType

checkExpr :: Expr -> TCMonad Type
checkExpr (Evar id) = getVarType id
checkExpr (ELitInt _) = return Int
checkExpr ELitTrue = return Bool
checkExpr ELitFalse = return Bool
checkExpr (EApp id exprs) = do
    FunInf (t, argTypes) <- getVarInf id
    when (length exprs /= length argTypes) $ throwError WrongNumberOfArguments
    let argTypesAndExprs = zip argTypes exprs
    mapM_ checkFunArg argTypesAndExprs
    return t
checkExpr (ArrRead id e) = do
    checkExprType e Int
    t <- getVarType id
    unless (checkIfArray t) $ throwError NotAnArray
    let (Array arrType) = t
    return arrType
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
    (env, oldVars, retType) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, oldVars, retType)
        _ -> throwError FunAlreadyDeclared

checkTopFun :: TCEnv -> FunDef -> TCMonad ()
checkTopFun initialEnv (FunDef t _ args block) = do
    put (initialEnv, S.empty, t)
    funArgsToEnv args
    checkBlock block

checkMain :: TCMonad ()
checkMain = do
    FunInf (t, argTypes) <- getVarInf $ Ident "main"
    when (t /= Int) $ throwError MainFunError
    when (argTypes /= []) $ throwError MainFunError

checkEveryTopFun :: [FunDef] -> TCMonad ()
checkEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    checkMain
    (initialEnv, _, _) <- get
    mapM_ (checkTopFun $ M.delete (Ident "main") initialEnv) fundefs

check :: Program -> IO (String, Bool)
check (Program fundefs) = do
    let runS = runStateT (checkEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("Error: " ++ show err, True)
        Right _ -> return ("Typecheck finished without errors", False)
