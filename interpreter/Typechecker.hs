module Typechecker (check) where

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

data TCInf = VarInf (Type, Bool) 
           | FunInf (Type, [Type])
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

-- ========================== Utils ===========================================

emptyState :: TCState
emptyState = (M.empty, S.empty, Void)

getItemIdent :: Item -> Ident
getItemIdent (NoInit _ id) = id
getItemIdent (Init _ id _) = id

checkIfArray :: Type -> Bool
checkIfArray (Array _ _) = True
checkIfArray _ = False

-- ========================== TCMonad utils ===================================

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

setNewOldVars :: TCMonad ()
setNewOldVars = do
    (env, _, retType) <- get
    let oldVars = S.fromAscList $ M.keys env
    put (env, oldVars, retType)

funArgsToEnv :: [Arg] -> TCMonad ()
funArgsToEnv = mapM_ (\(Arg _ argType argId) -> varToEnv argId argType False)

setRetType :: Type -> TCMonad ()
setRetType t = do
    (env, oldVars, _) <- get
    put (env, oldVars, t)

updateVarFinal :: Bool -> Ident -> TCMonad ()
updateVarFinal newFinal id = do
    VarInf (t, final) <- getVarInf id
    (env, oldVars, retType) <- get
    put (M.insert id (VarInf (t, newFinal)) env, oldVars, retType)

funToEnv :: FunDef -> TCMonad ()
funToEnv (FunDef _ t id args _) = do
    (env, oldVars, retType) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg _ t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, oldVars, retType)
        _ -> throwError FunAlreadyDeclared

checkType :: Type -> Type -> TCMonad ()
checkType expected actual = when (expected /= actual) $ throwError WrongType

checkExprType :: Expr -> Type -> TCMonad ()
checkExprType e t = do
    actualType <- checkExpr e
    checkType t actualType

-- ========================== Statements ======================================

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt _ block) = checkBlock block
checkStmt (Empty _) = return ()
checkStmt (FStmt _ fundef) = do
    funToEnv fundef
    checkFun fundef
checkStmt (ArrDecl _ t itms) = mapM_ (checkArrItem t) itms 
checkStmt (ArrAss _ id e1 e2) = do
    checkExprType e1 Int
    arrType <- getVarType id
    exprType <- checkExpr e2
    checkType arrType (Array exprType)
checkStmt (DStmt _ decl) = checkDecl decl
checkStmt (Ass _ id e) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkExprType e t
checkStmt (Inc _ id) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Dec _ id) = do
    VarInf (t, final) <- getVarInf id
    checkFinal final
    checkType Int t
checkStmt (Ret _ e) = do
    t <- checkExpr e
    checkReturn t
checkStmt (RetV _) = checkReturn Void
checkStmt (Break _) = return ()
checkStmt (Continue _) = return ()
checkStmt (Cond _ e block elifs) = do
    checkExprType e Bool
    checkBlockNewEnv block
    mapM_ checkElif elifs
checkStmt (CondElse _ e block1 elifs block2) = do
    checkExprType e Bool
    checkBlockNewEnv block1
    mapM_ checkElif elifs
    checkBlockNewEnv block2
checkStmt (While _ e block) = do
    checkExprType e Bool
    checkBlockNewEnv block
checkStmt (For _ init e exprs block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    finalVars <- checkForInit init
    checkExprType e Bool
    mapM_ (updateVarFinal False) finalVars
    mapM_ checkExpr exprs
    mapM_ (updateVarFinal True) finalVars
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
checkStmt (ForIn _ id1 id2 block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    t <- getVarType id2
    unless (checkIfArray t) $ throwError NotAnArray
    let (Array _ id1Type) = t
    varToEnv id1 id1Type True
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
checkStmt (EStmt _ e) = void $ checkExpr e
checkStmt (Print _ e) = void $ checkExpr e

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit _ id) = varToEnv id t final
checkItem t final (Init _ id e) = do
    checkExprType e t
    varToEnv id t final

checkDecl :: Decl -> TCMonad ()
checkDecl (NormalDecl _ t itms) = mapM_ (checkItem t False) itms
checkDecl (FinalDecl _ t itms) = mapM_ (checkItem t True) itms

checkBlock :: Block -> TCMonad ()
checkBlock (Block _ stmts) = mapM_ checkStmt stmts

checkBlockNewEnv :: Block -> TCMonad ()
checkBlockNewEnv block = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)

checkFinal :: Bool -> TCMonad ()
checkFinal final = when final $ throwError FinalVarAssignment

checkElif :: Elif -> TCMonad ()
checkElif (Elif _ e block) = do
    checkExprType e Bool
    checkBlockNewEnv block

checkForInit :: ForInit -> TCMonad [Ident]
checkForInit (ForInitExpr _ exprs) = do
    mapM_ checkExpr exprs
    return []
checkForInit (ForInitVar _ decl) = do
    checkStmt (DStmt decl)
    case decl of
        NormalDecl {} -> return []
        (FinalDecl _ _ itms) -> return $ map getItemIdent itms

checkArrItem :: Type -> ArrItem -> TCMonad ()
checkArrItem t (ArrNoInit _ id) = varToEnv id (Array t) False
checkArrItem t (ArrInit _ id e1 e2) = do
    checkExprType e1 Int
    checkExprType e2 t
    varToEnv id (Array t) False

checkReturn :: Type -> TCMonad ()
checkReturn t = do
    (_, _, retType) <- get
    when (t /= retType) $ throwError ReturnTypeError

checkFun :: FunDef -> TCMonad ()
checkFun (FunDef _ t id args block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    funArgsToEnv args
    setRetType t
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)

-- ========================== Expressions =====================================

checkExpr :: Expr -> TCMonad Type
checkExpr (Evar _ id) = getVarType id
checkExpr (ELitInt _ _) = return Int
checkExpr (ELitTrue _) = return Bool
checkExpr (ELitFalse _) = return Bool
checkExpr (EApp _ id exprs) = do
    FunInf (t, argTypes) <- getVarInf id
    when (length exprs /= length argTypes) $ throwError WrongNumberOfArguments
    let argTypesAndExprs = zip argTypes exprs
    mapM_ checkFunArg argTypesAndExprs
    return t
checkExpr (ArrRead _ id e) = do
    checkExprType e Int
    t <- getVarType id
    unless (checkIfArray t) $ throwError NotAnArray
    let (Array _ arrType) = t
    return arrType
checkExpr (EString _ _) = return Str
checkExpr (Neg _ e) = checkPrefixOpType e Int
checkExpr (Not _ e) = checkPrefixOpType e Bool
checkExpr (EMul _ e1 _ e2) = checkOpType e1 e2 Int
checkExpr (EAdd _ e1 _ e2) = checkOpType e1 e2 Int
checkExpr (EInc _ id) = checkPostfixOpType id
checkExpr (EDec _ id) = checkPostfixOpType id
checkExpr (ERel _ e1 _ e2) = do
    t <- checkExpr e1
    checkExprType e2 t
    return Bool
checkExpr (EAnd _ e1 e2) = checkOpType e1 e2 Bool
checkExpr (EOr _ e1 e2) = checkOpType e1 e2 Bool

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

-- ========================== Running =========================================

checkTopFun :: TCEnv -> FunDef -> TCMonad ()
checkTopFun initialEnv (FunDef _ t _ args block) = do
    put (initialEnv, S.empty, t)
    funArgsToEnv args
    checkBlock block

checkMain :: TCMonad ()
checkMain = do
    FunInf (t, argTypes) <- getVarInf $ Ident "main"
    when (t /= Int') $ throwError MainFunError
    when (argTypes /= []) $ throwError MainFunError

checkEveryTopFun :: [FunDef] -> TCMonad ()
checkEveryTopFun fundefs = do
    mapM_ funToEnv fundefs
    checkMain
    (initialEnv, _, _) <- get
    mapM_ (checkTopFun $ M.delete (Ident "main") initialEnv) fundefs

check :: Program -> IO (String, Bool)
check (Program _ fundefs) = do
    let runS = runStateT (checkEveryTopFun fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("Error: " ++ show err, True)
        Right _ -> return ("Typecheck finished without errors", False)
