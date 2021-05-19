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

data TCType = TCInt
            | TCStr
            | TCBool
            | TCVoid
 
data TCError = NoMainFunction
             | VarAlreadyDeclared Ident BNFC'Position
             | VarNotDeclared Ident BNFC'Position
             | FunAlreadyDeclared Ident BNFC'Position
             | WrongType Type Type BNFC'Position
             | WrongTCType TCType Type BNFC'Position
             | WrongMainType Type BNFC'Position
             | FinalVarAssignment Ident BNFC'Position
             | ReturnTypeError Type Type BNFC'Position
             | WrongArgumentType Type Type BNFC'Position
             | WrongNumberOfArguments Ident Int Int BNFC'Position
             | NotAnArray Ident BNFC'Position
    deriving Show

instance Show TCType where
    show TCInt = "int"
    show TCStr = "string"
    show TCBool = "bool"
    show TCVoid = "void"

-- ========================== Utils ===========================================

emptyState :: TCState
emptyState = (M.empty, S.empty, Void BNFC'NoPosition)

getItemIdent :: Item -> Ident
getItemIdent (NoInit _ id) = id
getItemIdent (Init _ id _) = id

checkIfArray :: Type -> Bool
checkIfArray (Array _ _) = True
checkIfArray _ = False

checkType :: Type -> Type -> Bool
checkType (Int _) (Int _) = True
checkType (Str _) (Str _) = True
checkType (Bool _) (Bool _) = True
checkType (Void _) (Void _) = True
checkType (Array _ t1) (Array _ t2) = checkType t1 t2
checkType _ _ = False

checkTCType :: TCType -> Type -> Bool
checkTCType TCInt (Int _) = True
checkTCType TCStr (Str _) = True
checkTCType TCBool (Bool _) = True
checkTCType TCVoid (Void _) = True
checkTCType _ _ = False

showT :: Type -> String
showT (Int _) = show TCInt
showT (Str _) = show TCStr
showT (Bool _) = show TCBool
showT (Void _) = show TCVoid
showT (Array _ t) = "Array<" ++ showT t ++ ">"

errMsgPrefix :: BNFC'Position -> String
errMsgPrefix p = case p of
    Nothing -> "Static Error: "
    Just (l, c) -> "Static Error (line " ++ show l ++ ", column " ++ show c ++ "): "

errMsg :: TCError -> String
errMsg NoMainFunction = errMsgPrefix Nothing ++
    "No main function"
errMsg (VarAlreadyDeclared id p) = errMsgPrefix p ++
    "Variable [" ++ show id ++ "] already declared"
errMsg (VarNotDeclared id p) = errMsgPrefix p ++
    "Variable/Function [" ++ show id ++ "] not declared"
errMsg (FunAlreadyDeclared id p) = errMsgPrefix p ++
    "Function [" ++ show id ++ "] already declared"
errMsg (WrongType exp act p) = errMsgPrefix p ++
    "Found type (" ++ showT act ++ ") instead of (" ++ showT exp ++ ")"
errMsg (WrongTCType exp act p) = errMsgPrefix p ++
    "Found type (" ++ showT act ++ ") instead of (" ++ show exp ++ ")"
errMsg (WrongMainType t p) = errMsgPrefix p ++
    "Main type should be (int) instead of (" ++ showT t ++ ")"
errMsg (FinalVarAssignment id p) = errMsgPrefix p ++
    "Cannot assign value to a read-only variable [" ++ show id ++ "]"
errMsg (ReturnTypeError exp act p) = errMsgPrefix p ++
    "Found return type (" ++ showT act ++ ") instead of (" ++ showT exp ++ ")"
errMsg (WrongArgumentType exp act p) = errMsgPrefix p ++
    "Found argument type (" ++ showT act ++ ") instead of (" ++ showT exp ++ ")"
errMsg (WrongNumberOfArguments id exp act p) = errMsgPrefix p ++
    "Function (" ++ show id ++ ") takes " ++ show exp ++ " arguments instead of " ++ show act

posFromType :: Type -> BNFC'Position
posFromType (Int p) = p
posFromType (Str p) = p
posFromType (Bool p) = p
posFromType (Void p) = p
posFromType (Array p _) = p

-- ========================== TCMonad utils ===================================

varToEnv :: Ident -> Type -> Bool -> BNFC'Position -> TCMonad ()
varToEnv id t final p = do
    (env, oldVars, retType) <- get
    when (M.member id env && S.notMember id oldVars) $ throwError $ VarAlreadyDeclared id p
    put (M.insert id (VarInf (t, final)) env, S.delete id oldVars, retType)

getVarInf :: Ident -> BNFC'Position -> TCMonad TCInf
getVarInf id p = do
    (env, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id p
        Just inf -> return inf

getVarType :: Ident -> BNFC'Position -> TCMonad Type
getVarType id p = do
    inf <- getVarInf id p
    case inf of
        VarInf (t, _) -> return t
        FunInf (t, _) -> return t

setNewOldVars :: TCMonad ()
setNewOldVars = do
    (env, _, retType) <- get
    let oldVars = S.fromAscList $ M.keys env
    put (env, oldVars, retType)

funArgsToEnv :: [Arg] -> TCMonad ()
funArgsToEnv = mapM_ (\(Arg p argType argId) -> varToEnv argId argType False p)

setRetType :: Type -> TCMonad ()
setRetType t = do
    (env, oldVars, _) <- get
    put (env, oldVars, t)

updateVarFinal :: BNFC'Position -> Bool -> Ident -> TCMonad ()
updateVarFinal p newFinal id = do
    VarInf (t, final) <- getVarInf id p
    (env, oldVars, retType) <- get
    put (M.insert id (VarInf (t, newFinal)) env, oldVars, retType)

funToEnv :: FunDef -> TCMonad ()
funToEnv (FunDef p t id args _) = do
    (env, oldVars, retType) <- get
    case M.lookup id env of
        Nothing -> do
            let argTypeList = foldr (\(Arg _ t _) acc -> t : acc) [] args
            put (M.insert id (FunInf (t, argTypeList)) env, oldVars, retType)
        _ -> throwError $ FunAlreadyDeclared id p

assertType :: Type -> Type -> BNFC'Position -> TCMonad ()
assertType expected actual p = 
    unless (checkType expected actual) $ throwError $ WrongType expected actual p

assertExprType :: Expr -> Type -> BNFC'Position -> TCMonad ()
assertExprType e t p = do
    actualType <- checkExpr e
    assertType t actualType p

assertTCType :: TCType -> Type -> BNFC'Position -> TCMonad ()
assertTCType expected actual p = 
    unless (checkTCType expected actual) $ throwError $ WrongTCType expected actual p

assertExprTCType :: Expr -> TCType -> BNFC'Position -> TCMonad ()
assertExprTCType e t p = do
    actualType <- checkExpr e
    assertTCType t actualType p

-- ========================== Statements ======================================

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt _ block) = checkBlock block
checkStmt (Empty _) = return ()
checkStmt (FStmt _ fundef) = do
    funToEnv fundef
    checkFun fundef
checkStmt (ArrDecl _ t itms) = mapM_ (checkArrItem t) itms 
checkStmt (ArrAss p id e1 e2) = do
    assertExprTCType e1 TCInt p
    arrType <- getVarType id p
    exprType <- checkExpr e2
    assertType arrType (Array p exprType) p
checkStmt (DStmt _ decl) = checkDecl decl
checkStmt (Ass p id e) = do
    VarInf (t, final) <- getVarInf id p
    checkFinal id final p
    assertExprType e t p
checkStmt (Inc p id) = do
    VarInf (t, final) <- getVarInf id p
    checkFinal id final p
    assertTCType TCInt t p
checkStmt (Dec p id) = do
    VarInf (t, final) <- getVarInf id p
    checkFinal id final p
    assertTCType TCInt t p
checkStmt (Ret p e) = do
    t <- checkExpr e
    checkReturn t p
checkStmt (RetV p) = checkReturn (Void p) p
checkStmt (Break _) = return ()
checkStmt (Continue _) = return ()
checkStmt (Cond p e block elifs) = do
    assertExprTCType e TCBool p
    checkBlockNewEnv block
    mapM_ checkElif elifs
checkStmt (CondElse p e block1 elifs block2) = do
    assertExprTCType e TCBool p
    checkBlockNewEnv block1
    mapM_ checkElif elifs
    checkBlockNewEnv block2
checkStmt (While p e block) = do
    assertExprTCType e TCBool p
    checkBlockNewEnv block
checkStmt (For p init e exprs block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    finalVars <- checkForInit init
    assertExprTCType e TCBool p
    mapM_ (updateVarFinal p False) finalVars
    mapM_ checkExpr exprs
    mapM_ (updateVarFinal p True) finalVars
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
checkStmt (ForIn p id1 id2 block) = do
    (oldEnv, oldOldVars, oldRetType) <- get
    setNewOldVars
    t <- getVarType id2 p
    unless (checkIfArray t) $ throwError $ NotAnArray id2 p
    let (Array _ id1Type) = t
    varToEnv id1 id1Type True p
    checkBlock block
    put (oldEnv, oldOldVars, oldRetType)
checkStmt (EStmt _ e) = void $ checkExpr e
checkStmt (Print _ e) = void $ checkExpr e

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit p id) = varToEnv id t final p
checkItem t final (Init p id e) = do
    assertExprType e t p
    varToEnv id t final p

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

checkFinal :: Ident -> Bool -> BNFC'Position -> TCMonad ()
checkFinal id final p = when final $ throwError $ FinalVarAssignment id p

checkElif :: Elif -> TCMonad ()
checkElif (Elif p e block) = do
    assertExprTCType e TCBool p
    checkBlockNewEnv block

checkForInit :: ForInit -> TCMonad [Ident]
checkForInit (ForInitExpr _ exprs) = do
    mapM_ checkExpr exprs
    return []
checkForInit (ForInitVar _ decl) = do
    checkDecl decl
    case decl of
        NormalDecl {} -> return []
        (FinalDecl _ _ itms) -> return $ map getItemIdent itms

checkArrItem :: Type -> ArrItem -> TCMonad ()
checkArrItem t (ArrNoInit p id) = varToEnv id (Array BNFC'NoPosition t) False p
checkArrItem t (ArrInit p id e1 e2) = do
    assertExprTCType e1 TCInt p
    assertExprType e2 t p
    varToEnv id (Array BNFC'NoPosition t) False p

checkReturn :: Type -> BNFC'Position -> TCMonad ()
checkReturn t p = do
    (_, _, retType) <- get
    unless (checkType t retType) $ throwError $ ReturnTypeError t retType p

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
checkExpr (Evar p id) = getVarType id p
checkExpr (ELitInt p _) = return $ Int p
checkExpr (ELitTrue p) = return $ Bool p
checkExpr (ELitFalse p) = return $ Bool p
checkExpr (EApp p id exprs) = do
    FunInf (t, argTypes) <- getVarInf id p
    let argLen = length argTypes
    let exprLen = length exprs
    when (exprLen /= argLen) $ throwError $ WrongNumberOfArguments id argLen exprLen p
    let argTypesAndExprs = zip argTypes exprs
    mapM_ (checkFunArg p) argTypesAndExprs
    return t
checkExpr (ArrRead p id e) = do
    assertExprTCType e TCInt p
    t <- getVarType id p
    unless (checkIfArray t) $ throwError $ NotAnArray id p
    let (Array _ arrType) = t
    return arrType
checkExpr (EString p _) = return (Str p)
checkExpr (Neg p e) = checkPrefixOpType e (Int p)
checkExpr (Not p e) = checkPrefixOpType e (Bool p)
checkExpr (EMul p e1 _ e2) = checkOpType e1 e2 (Int p)
checkExpr (EAdd p e1 _ e2) = checkOpType e1 e2 (Int p)
checkExpr (EInc p id) = checkPostfixOpType id p
checkExpr (EDec p id) = checkPostfixOpType id p
checkExpr (ERel p e1 _ e2) = do
    t <- checkExpr e1
    assertExprType e2 t p
    return (Bool p)
checkExpr (EAnd p e1 e2) = checkOpType e1 e2 (Bool p)
checkExpr (EOr p e1 e2) = checkOpType e1 e2 (Bool p)

checkOpType :: Expr -> Expr -> Type -> TCMonad Type
checkOpType e1 e2 t = do
    assertExprType e1 t (posFromType t)
    assertExprType e2 t (posFromType t)
    return t

checkPostfixOpType :: Ident -> BNFC'Position -> TCMonad Type
checkPostfixOpType id p = do
    VarInf (t, final) <- getVarInf id p
    checkFinal id final p
    assertTCType TCInt t p
    return t

checkPrefixOpType :: Expr -> Type -> TCMonad Type
checkPrefixOpType e t = do
    assertExprType e t (posFromType t)
    return t

checkFunArg :: BNFC'Position -> (Type, Expr) -> TCMonad ()
checkFunArg p (t, e) = do
    exprType <- checkExpr e
    unless (checkType t exprType) $ throwError $ WrongArgumentType t exprType p

-- ========================== Running =========================================

checkTopFun :: TCEnv -> FunDef -> TCMonad ()
checkTopFun initialEnv (FunDef _ t _ args block) = do
    put (initialEnv, S.empty, t)
    funArgsToEnv args
    checkBlock block

checkIfInt :: Type -> Bool
checkIfInt (Int _) = True
checkIfInt _ = False

checkMain :: TCMonad ()
checkMain = do
    let id = Ident "main"
    (env, _, _) <- get
    case M.lookup id env of
        Nothing -> throwError NoMainFunction
        Just (FunInf (t, argTypes)) -> do
            unless (checkTCType TCInt t) $ throwError $ WrongMainType t (posFromType t)
            unless (null argTypes) $ throwError $ WrongNumberOfArguments id (length argTypes) 0 (posFromType $ head argTypes)

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
        Left err -> return (errMsg err, True)
        Right _ -> return ("", False)
