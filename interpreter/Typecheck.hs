module Typecheck where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import qualified Data.Map as M

type TCEnv = M.Map Ident (Type, Bool)
type TCExcept = ExceptT TCError IO
type TCMonad = StateT TCEnv TCExcept

data TCError = VarAlreadyDeclared
             | VarNotDeclared

idToEnv :: Ident -> Type -> Bool -> TCMonad ()
idToEnv id t final = do
    env <- get
    put (M.insert id (t, final) env)

checkItem :: Type -> Bool -> Item -> TCMonad ()
checkItem t final (NoInit id) = idToEnv id t final
checkItem t final (Init id expr) = undefined

checkDecl :: Decl -> TCMonad ()
checkDecl (NormalDecl t items) = undefined
checkDecl (FinalDecl t items) = undefined

checkStmt :: Stmt -> TCMonad ()
checkStmt (BStmt block) = undefined
checkStmt (DStmt decl) = undefined

checkExpr :: Expr -> TCMonad Type
checkExpr = undefined
