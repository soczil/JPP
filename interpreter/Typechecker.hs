module Typechecker where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import qualified Data.Map as M

type TCVarInf = (Type, Bool)
type TCEnv = M.Map Ident TCVarInf
type TCExcept = ExceptT TCError IO
type TCMonad = StateT TCEnv TCExcept

data TCError = VarAlreadyDeclared
             | VarNotDeclared Ident
             | WrongType

varToEnv :: Ident -> Type -> Bool -> TCMonad ()
varToEnv id t final = do
    env <- get
    put (M.insert id (t, final) env)

getVarInf :: Ident -> TCMonad TCVarInf
getVarInf id = do
    env <- get
    case M.lookup id env of
        Nothing -> throwError $ VarNotDeclared id
        Just inf -> return inf

getVarType :: Ident -> TCMonad Type
getVarType id = do
    (t, _) <- getVarInf id
    return t

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit id) = varToEnv id t final
checkItem t final (Init id expr) = undefined

checkDecl :: Decl -> TCMonad ()
checkDecl (NormalDecl t items) = undefined
checkDecl (FinalDecl t items) = undefined

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt block) = undefined
checkStmt (DStmt decl) = undefined

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
    (t, _) <- getVarInf id
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
