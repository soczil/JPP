module Interpreter (interpret) where

import Control.Monad.State
import Control.Monad.Except

import Lattepp.Abs

import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import qualified Data.Map as M

type Loc = Int
type LPPEnv = M.Map Ident Loc
type LPPStore = M.Map Loc Value
type LPPState = (LPPStore, LPPEnv, Loc, Bool, Maybe Value, Maybe LPPLoop)
type LPPExcept = ExceptT LPPError IO
type LPPMonad = StateT LPPState LPPExcept

data Value = VInt Integer
           | VString String
           | VBool Bool
           | VArray [Value]
           | VFun (Type, [Ident], Block, LPPEnv)
           | VVoid
    deriving (Ord, Eq)

data LPPLoop = LPPContinue
             | LPPBreak

data LPPError =  DivisionByZero 
               | ModuloByZero 
               | IndexOutOfBounds  
    deriving Show

-- ========================== Utils ===========================================

emptyState :: LPPState
emptyState = (M.empty, M.empty, 0, False, Nothing, Nothing)

valueToString :: Value -> String
valueToString (VInt x) = show x
valueToString (VString s) = s
valueToString (VBool x) = show x
valueToString (VArray a) = "[" ++ intercalate "," (foldl (\acc x -> acc ++ [valueToString x]) [] a) ++ "]"

defaultValue :: Type -> Value
defaultValue Int = VInt 0
defaultValue Bool = VBool False
defaultValue Str = VString ""

continueLoopFlag :: Maybe LPPLoop -> Bool
continueLoopFlag (Just LPPContinue) = True
continueLoopFlag _ = False

-- ========================== LPPMonad utils ==================================

varToEnv :: Ident -> LPPMonad ()
varToEnv id = do
    (store, env, loc, elseFlag, retVal, loopFlag) <- get
    let updatedEnv = M.insert id loc env
    put (store, updatedEnv, loc + 1, elseFlag, retVal, loopFlag)

getVarLoc :: Ident -> LPPMonad Loc
getVarLoc id = do
    (_, env, _, _, _, _) <- get
    return $ fromJust (M.lookup id env)

getVarValue :: Ident -> LPPMonad Value
getVarValue id = do
    loc <- getVarLoc id
    (store, _, _, _, _, _) <- get
    return $ fromJust (M.lookup loc store)

updateVarValue :: Ident -> Value -> LPPMonad ()
updateVarValue id val = do
    varLoc <- getVarLoc id
    (store, env, loc, elseFlag, retVal, loopFlag) <- get
    let updatedStore = M.insert varLoc val store
    put (updatedStore, env, loc, elseFlag, retVal, loopFlag)

getElseFlag :: LPPMonad Bool
getElseFlag = do
    (_, _, _, doElse, _, _) <- get 
    return doElse

setElseFlag :: Bool -> LPPMonad ()
setElseFlag val = do
    (store, env, loc, _, retVal, loopFlag) <- get
    put (store, env, loc, val, retVal, loopFlag)

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
    (_, env, _, _, _, _) <- get
    return env

setEnv :: LPPEnv -> LPPMonad ()
setEnv env = do
    (store, _, loc, elseFlag, retVal, loopFlag) <- get
    put (store, env, loc, elseFlag, retVal, loopFlag)

getRetVal :: LPPMonad (Maybe Value)
getRetVal = do
    (_, _, _, _, retVal, _) <- get
    return retVal

setRetVal :: Maybe Value -> LPPMonad ()
setRetVal retVal = do
    (store, env, loc, elseFlag, _, loopFlag) <- get
    put (store, env, loc, elseFlag, retVal, loopFlag)

getLoopFlag :: LPPMonad (Maybe LPPLoop)
getLoopFlag = do
    (_, _, _, _, _, loopFlag) <- get
    return loopFlag

setLoopFlag :: Maybe LPPLoop -> LPPMonad ()
setLoopFlag loopFlag = do
    (store, env, loc, elseFlag, retVal, _) <- get
    put (store, env, loc, elseFlag, retVal, loopFlag)

getArrElem :: [Value] -> Integer -> LPPMonad Value
getArrElem arr idx = getArrElemAux arr idx 0
    where
        getArrElemAux :: [Value] -> Integer -> Integer -> LPPMonad Value
        getArrElemAux [] _ _ = throwError IndexOutOfBounds
        getArrElemAux (x:xs) idx currIdx = 
            if idx == currIdx
                then return x
                else getArrElemAux xs idx (currIdx + 1)

funArgToState :: (Ident, Value) -> LPPMonad ()
funArgToState (id, val) = do
    varToEnv id
    updateVarValue id val

funToEnv :: FunDef -> LPPMonad ()
funToEnv (FunDef _ id _ _) = do
    varToEnv id

funToStore :: LPPEnv -> FunDef -> LPPMonad ()
funToStore env (FunDef t id args block) = do
    let argIds = foldr (\(Arg _ argId) acc -> argId : acc) [] args
    updateVarValue id $ VFun (t, argIds, block, env)

-- ========================== Statements ======================================

execStmt :: Stmt -> LPPMonad ()
execStmt (BStmt block) = execBlockNewEnv block
execStmt Empty = return ()
execStmt (FStmt fundef) = do
    funToEnv fundef
    env <- getEnv
    funToStore env fundef
execStmt (ArrDecl _ itms) = mapM_ execArrItem itms
execStmt (ArrAss id e1 e2) = do
    VInt idx <- evalExpr e1
    val <- evalExpr e2
    VArray arr <- getVarValue id
    updatedArr <- updateArr arr idx val
    updateVarValue id updatedArr
execStmt (DStmt decl) =
    case decl of
        (NormalDecl t itms) -> mapM_ (execItem t) itms
        (FinalDecl t itms) -> mapM_ (execItem t) itms 
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
execStmt Break = setLoopFlag $ Just LPPBreak
execStmt Continue = setLoopFlag $ Just LPPContinue
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
    oldLoopFlag <- getLoopFlag
    execLoop e block []
    setEnv oldEnv
    setLoopFlag oldLoopFlag
execStmt (For init e exprs block) = do
    oldEnv <- getEnv
    oldLoopFlag <- getLoopFlag
    execForInit init
    execLoop e block exprs
    setEnv oldEnv
    setLoopFlag oldLoopFlag
execStmt (ForIn id1 id2 block) = do
    oldEnv <- getEnv
    oldLoopFlag <- getLoopFlag
    VArray arr <- getVarValue id2
    varToEnv id1
    execForInLoop id1 block arr
    setEnv oldEnv
    setLoopFlag oldLoopFlag
execStmt (EStmt expr) = void $ evalExpr expr
execStmt (Print e) = do
    val <- evalExpr e
    liftIO $ putStrLn $ valueToString val

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

execLoopBlock :: Block -> LPPMonad ()
execLoopBlock (Block stmts) = mapM_ execLoopBlockAux stmts
    where
        execLoopBlockAux stmt = do
            retVal <- getRetVal
            loopFlag <- getLoopFlag
            when (isNothing retVal && isNothing loopFlag) $ execStmt stmt

execBlockNewEnv :: Block -> LPPMonad ()
execBlockNewEnv block = do
    oldEnv <- getEnv
    execBlock block
    setEnv oldEnv

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

execForInit :: ForInit -> LPPMonad ()
execForInit (ForInitExpr exprs) = mapM_ evalExpr exprs
execForInit (ForInitVar decl) = execStmt (DStmt decl)

execLoop :: Expr -> Block -> [Expr] -> LPPMonad ()
execLoop e1 block exprs = do
    setLoopFlag Nothing
    VBool val1 <- evalExpr e1
    when val1 $ do
        execLoopBlock block
        loopFlag <- getLoopFlag
        when (isNothing loopFlag || continueLoopFlag loopFlag) $ do
            mapM_ evalExpr exprs
            execLoop e1 block exprs

execForInLoop :: Ident -> Block -> [Value] -> LPPMonad ()
execForInLoop _ _ [] = return ()
execForInLoop id block (val:vals) = do
    setLoopFlag Nothing
    updateVarValue id val
    execLoopBlock block
    loopFlag <- getLoopFlag
    when (isNothing loopFlag || continueLoopFlag loopFlag) $ do
        execForInLoop id block vals

-- ========================== Expressions =====================================

evalExpr :: Expr -> LPPMonad Value
evalExpr (Evar id) = getVarValue id 
evalExpr (ELitInt x) = return $ VInt x
evalExpr ELitTrue = return $ VBool True
evalExpr ELitFalse = return $ VBool False
evalExpr (EApp id exprs) = do
    vals <- mapM evalExpr exprs
    VFun (t, argIds, block, env) <- getVarValue id
    let argIdsAndVals = zip argIds vals
    (_, oldEnv,  _, oldElseFlag, oldRetVal, oldLoopFlag) <- get
    setEnv env
    mapM_ funArgToState argIdsAndVals
    execBlock block
    (store, _, loc, _, retVal, _) <- get
    put (store, oldEnv, loc, oldElseFlag, oldRetVal, oldLoopFlag)
    case retVal of
        Just val -> return val
        Nothing -> return $ defaultValue t
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

-- ========================== Running =========================================

execMain :: LPPMonad ()
execMain = do
    VFun (_, _, block, _) <- getVarValue $ Ident "main"
    execBlock block

interpretProgram :: [FunDef] -> LPPMonad ()
interpretProgram fundefs = do
    mapM_ funToEnv fundefs
    env <- getEnv
    mapM_ (funToStore $ M.delete (Ident "main") env) fundefs
    execMain

interpret :: Program -> IO (String, Int)
interpret (Program fundefs) = do
    let runS = runStateT (interpretProgram fundefs) emptyState
    result <- runExceptT runS
    case result of
        Left err -> return ("Error: " ++ show err, 1)
        Right msg -> return ("NoError", 0)
