module Interpreter where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Lattepp.Abs
import qualified Data.Map as M
import Data.List (intercalate)
import Data.Maybe (isNothing)

type Var = String 
type Loc = Int
type Env = M.Map Var Loc
type LPPEnv = M.Map Ident Loc
type LPPStore = M.Map Loc Value
type LPPState = (LPPStore, LPPEnv, Loc, Bool, Maybe Value)
type LPPExcept = ExceptT LPPError IO
type LPPReader = ReaderT Env LPPExcept
type LPPMonad = StateT LPPState LPPReader

data Value = VInt Integer 
           | VString String 
           | VBool Bool 
           | VArray [Value]
           | VFun (Type, [Arg], Block)
           | VVoid
    deriving (Ord, Eq)

data LPPError =  DivisionByZero 
               | ModuloByZero 
               | VarAlreadyDeclared 
               | VarNotDeclared 
               | IndexOutOfBounds 
               | UnknownError 
    deriving Show

emptyState :: LPPState
emptyState = (M.empty, M.empty, 0, False, Nothing)

funToState :: LPPState -> FunDef -> LPPState
funToState (store, env, loc, elseFlag, retVal) (FunDef t id args block) =
    (
        M.insert loc (VFun (t, args, block)) store,
        M.insert id loc env,
        loc + 1,
        elseFlag,
        retVal
    )      

newEnv :: Env
newEnv = M.empty

valueToString :: Value -> String
valueToString (VInt x) = show x
valueToString (VString s) = s
valueToString (VBool x) = show x
valueToString (VArray a) = "[" ++ intercalate "," (foldl (\acc x -> acc ++ [valueToString x]) [] a) ++ "]"

defaultValue :: Type -> Value
defaultValue Int = VInt 0
defaultValue Bool = VBool False
defaultValue Str = VString ""

varToEnv :: Ident -> LPPMonad ()
varToEnv id = do
    (store, env, loc, doElse, retVal) <- get
    let updatedEnv = M.insert id loc env
    put (store, updatedEnv, loc + 1, doElse, retVal)

getVarLoc :: Ident -> LPPMonad Loc
getVarLoc id = do
    (_, env, _, _, _) <- get
    case M.lookup id env of
        Just loc -> return loc
        Nothing -> throwError VarNotDeclared

getVarValue :: Ident -> LPPMonad Value
getVarValue id = do
    loc <- getVarLoc id
    (store, _, _, _, _) <- get
    case M.lookup loc store of
        Just val -> return val
        Nothing -> throwError UnknownError

updateVarValue :: Ident -> Value -> LPPMonad ()
updateVarValue id val = do
    varLoc <- getVarLoc id
    (store, env, loc, doElse, retVal) <- get
    let updatedStore = M.insert varLoc val store
    put (updatedStore, env, loc, doElse, retVal)

execItem :: Type -> Item -> LPPMonad ()
execItem t (NoInit id) = do
    varToEnv id
    updateVarValue id $ defaultValue t
execItem _ (Init id e) = do
    val <- evalExpr e
    varToEnv id
    updateVarValue id val

execBlock :: Block -> LPPMonad ()
execBlock (Block stmts) = mapM_ execBlockStmt stmts
    where
        execBlockStmt stmt = do
        retVal <- getRetVal
        when (isNothing retVal) $ execStmt stmt

execBlockNewEnv :: Block -> LPPMonad ()
execBlockNewEnv block = do
    oldEnv <- getEnv
    execBlock block
    setEnv oldEnv

getElseFlag :: LPPMonad Bool
getElseFlag = do
    (_, _, _, doElse, _) <- get 
    return doElse

setElseFlag :: Bool -> LPPMonad ()
setElseFlag val = do
    (store, env, loc, _, retVal) <- get
    put (store, env, loc, val, retVal)

execElif :: Elif -> LPPMonad ()
execElif (Elif e block) = do
    doElse <- getElseFlag
    VBool val <- evalExpr e
    when (doElse && val) $ do
        execBlockNewEnv block
        setElseFlag False

execArrItem :: ArrItem -> LPPMonad ()
execArrItem (ArrNoInit id) = do
    varToEnv id
    updateVarValue id $ VArray []
execArrItem (ArrInit id e1 e2) = do
    VInt len <- evalExpr e1
    val <- evalExpr e2
    let arr = take (fromInteger len) [val | x <- [0..]]
    varToEnv id
    updateVarValue id (VArray arr)

updateArr :: [Value] -> Integer -> Value -> LPPMonad Value
updateArr arr idx val = do
    let (updatedArr, updated, _) = foldl (\(newArr, updated, currIdx) x -> 
            if currIdx == idx
                then (newArr ++ [val], True, currIdx + 1)
                else (newArr ++ [x], updated, currIdx + 1)
                ) ([], False, 0) arr
    unless updated $ throwError IndexOutOfBounds
    return $ VArray updatedArr

getEnv :: LPPMonad LPPEnv
getEnv = do
    (_, env, _, _, _) <- get
    return env

setEnv :: LPPEnv -> LPPMonad ()
setEnv env = do
    (store, _, loc, elseFlag, retVal) <- get
    put (store, env, loc, elseFlag, retVal)

getRetVal :: LPPMonad (Maybe Value)
getRetVal = do
    (_, _, _, _, retVal) <- get
    return retVal

setRetVal :: Maybe Value -> LPPMonad ()
setRetVal retVal = do
    (store, env, loc, elseFlag, _) <- get
    put (store, env, loc, elseFlag, retVal)

execForInit :: ForInit -> LPPMonad ()
execForInit (ForInitExpr exprs) = mapM_ evalExpr exprs
execForInit (ForInitVar decl) = execStmt (DStmt decl)

execLoop :: Expr -> Block -> [Expr] -> LPPMonad ()
execLoop e1 block exprs = do
    VBool val1 <- evalExpr e1
    when val1 $ do
        execBlock block
        mapM_ evalExpr exprs
        execLoop e1 block exprs

execStmt :: Stmt -> LPPMonad ()
execStmt (BStmt block) = execBlockNewEnv block
execStmt Empty = return ()
execStmt (FStmt fundef) = undefined 
execStmt (ArrDecl t itms) = mapM_ execArrItem itms
execStmt (ArrAss id e1 e2) = do
    VInt idx <- evalExpr e1
    val <- evalExpr e2
    VArray arr <- getVarValue id
    updatedArr <- updateArr arr idx val
    updateVarValue id updatedArr
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
execStmt (Ret e) = do
    val <- evalExpr e
    setRetVal $ Just val
execStmt RetV = setRetVal $ Just VVoid
execStmt (Cond e block elifs) = do
    VBool val <- evalExpr e
    if val 
        then execBlockNewEnv block
        else do
            oldFlag <- getElseFlag
            setElseFlag True
            mapM_ execElif elifs
            setElseFlag oldFlag
execStmt (CondElse e block1 elifs block2) = do
    VBool val <- evalExpr e
    if val
        then execBlockNewEnv block1
        else do
            oldFlag <- getElseFlag
            setElseFlag True
            mapM_ execElif elifs
            doElse <- getElseFlag
            when doElse $ execBlockNewEnv block2
            setElseFlag oldFlag
execStmt (While e block) = do
    oldEnv <- getEnv
    execLoop e block []
    setEnv oldEnv
execStmt (For init e1 e2 block) = do
    oldEnv <- getEnv
    execForInit init
    execLoop e1 block e2
    setEnv oldEnv
execStmt (EStmt expr) = do
    evalExpr expr
    return ()
execStmt (Print e) = do
    val <- evalExpr e
    liftIO $ putStrLn $ valueToString val

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

getArrElem :: [Value] -> Integer -> LPPMonad Value
getArrElem arr idx = getArrElemAux arr idx 0
    where
        getArrElemAux :: [Value] -> Integer -> Integer -> LPPMonad Value
        getArrElemAux [] _ _ = throwError IndexOutOfBounds
        getArrElemAux (x:xs) idx currIdx = 
            if idx == currIdx
                then return x
                else getArrElemAux xs idx (currIdx + 1)

argToState :: (Arg, Value) -> LPPMonad ()
argToState (Arg _ id, val) = do
    varToEnv id
    updateVarValue id val

evalExpr :: Expr -> LPPMonad Value
evalExpr (Evar id) = getVarValue id 
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EApp id exprs) = do
    vals <- mapM evalExpr exprs
    VFun (t, args, block) <- getVarValue id
    let argsAndVals = zip args vals
    oldEnv <- getEnv
    mapM_ argToState argsAndVals
    execBlock block
    setEnv oldEnv
    retVal <- getRetVal
    case retVal of
        Just val -> do
            setRetVal Nothing
            return val
        Nothing -> return VVoid
evalExpr (ArrRead id e) = do
    VInt idx <- evalExpr e
    VArray arr <- getVarValue id
    getArrElem arr idx
evalExpr (EString s) = return $ VString s
evalExpr (Neg e) = do
    VInt val <- evalExpr e
    return $ VInt (-val)
evalExpr (Not e) = do
    VBool val <- evalExpr e
    return $ VBool (not val)
evalExpr (EMul e1 mulOp e2) = do
    VInt val1 <- evalExpr e1
    VInt val2 <- evalExpr e2
    evalMulOp mulOp val1 val2
evalExpr (EAdd e1 addOp e2) = do
    VInt val1 <- evalExpr e1
    VInt val2 <- evalExpr e2
    evalAddOp addOp val1 val2
evalExpr (EInc id) = do
    VInt val <- getVarValue id
    updateVarValue id $ VInt (val + 1)
    return $ VInt val
evalExpr (EDec id) = do
    VInt val <- getVarValue id
    updateVarValue id $ VInt (val - 1)
    return $ VInt val
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

execMain :: LPPMonad ()
execMain = do
    VFun (_, _, block) <- getVarValue $ Ident "main"
    execBlock block

interpret :: Program -> IO (String, Int)
interpret (Program fundefs) = do
    let newState = foldl funToState emptyState fundefs
    let runS = runStateT execMain newState
    let runR = runReaderT runS newEnv
    result <- runExceptT runR
    case result of
        Left err -> do
            return ("Error: " ++ show err, 1)
        Right msg -> do
            return ("NoError", 0)
