-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  ?
-- Portability :  ?
--
-- |Evaluation monad for tracer
--
-----------------------------------------------------------------------------

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Tracy.Eval where

import Tracy.Ast
import Tracy.AstExec hiding (not, and, or)
import Tracy.AstPretty
import Tracy.Types hiding (repairVar)
import qualified Tracy.TraceAst as TA

import qualified BitVector as BV

import Control.Monad.Writer hiding (tell)
import qualified Control.Monad.Writer as W
import Control.Monad.State
import Control.Monad.Reader
import Debug.Trace (trace)

import qualified Data.Map as M

-- |Making an Maybe an Eval
evalMaybe :: String -> Maybe a -> Eval a
evalMaybe e Nothing = error e
evalMaybe _ (Just a) = return a

-- Used for debugging.
--tell a | trace (show a) False = undefined 
tell a = W.tell a

-------------------------------------------------------------------------------
-- ** Eval monad
-------------------------------------------------------------------------------

newtype Eval a = E { runE :: StateT FunState (WriterT EvalTrace (ReaderT FunEnv IO)) a }
    deriving (Monad, MonadIO, MonadReader FunEnv, MonadWriter EvalTrace, MonadState FunState)

runEval :: Eval a -> FunState -> FunEnv -> IO ((a, FunState), EvalTrace)
runEval eval store funenv = runReaderT (runWriterT (runStateT (runE eval) store)) funenv 

-------------------------------------------------------------------------------
-- ** Creating an output trace
-------------------------------------------------------------------------------
type EvalTrace  = TA.Trace
type EvalAction = Action

emptyEvalTrace :: EvalTrace
emptyEvalTrace = []

tellIfTracing :: EvalAction -> String -> Eval()
tellIfTracing a "" =
  do  t <- isTracing
      if t
        then tell [a]
        else tell [Error a]
tellIfTracing a b =
  do  t <- isTracing
      if t
			  then tell [a, Debug b]
			  else tell [Error a, Debug b]

tracePrePost :: String -> Expr -> Eval ()
tracePrePost prePost expr = tell []

traceDebug :: String -> Eval ()
traceDebug s = tell [Debug s]

traceAssign :: Ident -> Expr -> Eval ()
traceAssign ident expr = do
    prefix <- getPrefix
    i      <- lookupFName ident
    v      <- lookupValue ident
    tellIfTracing (Assign prefix i posExpr) (i ++ " := " ++ pretty v)
  where 
    posExpr = 
      if   isBoolExpr expr
        then CondExpr expr (ConstExpr Signed 1) (ConstExpr Signed 0)
        else expr

traceType :: Ident -> Eval ()
traceType ident =
  do  prefix <- getPrefix
      i   <- lookupFName ident
      v   <- lookupValue ident
      tellIfTracing (Decl prefix (valueToType v) i) ""

traceCond :: Expr -> Value -> Eval()
traceCond expr (VVal _ _ 1) = 
  do  prefix <- getPrefix
      tellIfTracing (Assume prefix posExpr) ""
  where 
    posExpr = 
      if   isBoolExpr expr
      then expr
      else BinOpExpr expr Neq FalseExpr
traceCond expr _        = 
  do  prefix <- getPrefix
      tellIfTracing (Assume prefix negExpr) ""
  where 
    negExpr = 
      if   isBoolExpr expr
      then UnOpExpr Not expr
      else BinOpExpr expr Neq TrueExpr

traceAssert :: Expr -> Value -> Eval()
traceAssert expr (VVal _ _ 1) = 
  do  prefix <- getPrefix
      tellIfTracing (Assert prefix posExpr) ""
  where 
    posExpr = 
      if   isBoolExpr expr
      then expr
      else BinOpExpr expr Neq FalseExpr
traceAssert expr _        = 
  do  prefix <- getPrefix
      tellIfTracing (Assert prefix negExpr) ""
  where 
    negExpr = 
      if   isBoolExpr expr
      then UnOpExpr Not expr
      else BinOpExpr expr Neq TrueExpr

traceRepairVariable :: Eval()
traceRepairVariable = 
  do  prefix <- getPrefix
      traceRV prefix =<< getRepairVariable
  where
    traceRV _       Nothing     = return ()
    traceRV prefix (Just ident) =
      do val <- lookupValue ident
         case val of
           (VVal _ _ _) -> tell [Assign prefix "controllable" (VarExpr $ "controllable_" ++ ident)]
           (_rest )     -> error "Repair variable is not assigned."


-------------------------------------------------------------------------------
-- ** State of evaluation
-------------------------------------------------------------------------------

type VarState = (Ident, M.Map Ident (Value, Ident))
data FunState = FunState { globalvars :: VarState
                         , localvars  :: VarState
                         , stack      :: [VarState]
                         , tracing    :: Bool
                         , error_seen :: Bool 
                         , callExprs  :: [Expr]
                         , returnExpr :: Maybe Expr 
                         , freshVar   :: Int
                         }
              deriving (Eq, Show)

emptyState :: FunState
emptyState = FunState { globalvars = ("",M.empty), localvars = ("",M.empty), stack = [], tracing = True, error_seen = False, 
                        returnExpr = Nothing, callExprs = [], freshVar = 1}

setTopFun :: Ident -> Eval ()
setTopFun ident = 
  do  funState <- get 
      let (_,g) = globalvars funState
      put $ funState { globalvars = (ident, g) }

setLocalFun :: Ident -> Eval ()
setLocalFun ident = 
  do  funState <- get 
      let (_,g) = localvars funState
      put $ funState { localvars = (ident, g) }

showState :: Eval String
showState = liftM show get

toggleTracing :: Eval ()
toggleTracing = 
    do  funState <- get
        put $ funState { tracing = not (tracing funState) }

isTracing :: Eval Bool
isTracing = liftM tracing get

setErrorSeen :: Eval ()
setErrorSeen = 
    do  funState <- get
        put $ funState { error_seen = True }

getPrefix :: Eval String
getPrefix = 
  do  funState <- get 
      if error_seen funState
        then return "post"
        else return "pre"

isSetReturnExpr :: Eval Bool
isSetReturnExpr = 
  do  funState <- get 
      case returnExpr funState of
        Nothing   -> return False
        otherwise -> return True

unsetReturnExpr :: Eval ()
unsetReturnExpr = 
  do  funState <- get 
      put $ funState { returnExpr = Nothing }

getReturnExpr :: Eval (Maybe Expr)
getReturnExpr = liftM returnExpr get

setReturnExpr :: Expr -> Eval ()
setReturnExpr expr = 
  do  funState <- get 
      put $ funState { returnExpr = Just(expr) }

getCallExprs :: Eval [Expr]
getCallExprs = liftM callExprs get

setCallExprs :: [Expr] -> Eval ()
setCallExprs expr = 
  do  funState <- get 
      put $ funState { callExprs = expr }

getFresh :: Eval Int
getFresh = state $ \funState -> (freshVar funState, funState { freshVar = (freshVar funState) + 1 })

lookupValue :: Ident -> Eval Value
lookupValue ident = 
    do funState <- get
       v <- evalMaybe ("Variable does not exist: " ++ ident) $ l funState
       return $ fst v
    where
        l fs = case M.lookup ident (snd $ localvars fs) of
            Nothing -> M.lookup ident (snd $ globalvars fs)
            Just(v) -> Just(v)

lookupFName :: Ident -> Eval Ident
lookupFName ident = 
    do funState <- get
       v <- evalMaybe ("Variable in lookup does not exist" ++ ident) $ l funState
       b <- isPerformingSSA
       if   and [(fst $ globalvars funState) == (fst $ localvars funState), not b]
         then return $ ident
         else return $ snd v
    where
        l fs = case M.lookup ident (snd $ localvars fs) of
            Nothing -> M.lookup ident (snd $ globalvars fs)
            Just(v) -> Just(v)

initGlobalValue :: Ident -> Type -> Eval()
initGlobalValue ident tp =
  do funState <- get
     s <- evalMaybe ("Variable " ++ ident ++ " already created") $ l funState
     put s
     where
        l fs = if   M.member ident (snd $ globalvars fs)
                 then Nothing
                 else Just $ fs { globalvars = (fst $ globalvars fs, M.insert ident (VUnassigned tp, ident) (snd $ globalvars fs)) }

initArray :: Ident -> Type -> Array -> Eval()
initArray ident (TVal t p) a = 
  do  funState <- get
      let _ = created $ M.lookup ident (snd $ localvars funState)
      freshV <- getFresh
      funState2 <- get
      let localFun = fst $ localvars funState2
      put $ funState2 { localvars = (localFun, M.insert ident ((VArray t p a), localFun ++ "_" ++ ident ++ "_" ++ show freshV) (snd $ localvars funState2)) }
  where 
     created Nothing  = Nothing
     created (Just _) = error $ "Variable " ++ ident ++ " already created"

initValue :: Ident -> Type -> Eval()
initValue ident tp =  
  do  funState <- get
      let _ = created $ M.lookup ident (snd $ localvars funState)
      freshV <- getFresh
      funState2 <- get
      let localFun = fst $ localvars funState2
      put $ funState2 { localvars = (localFun, M.insert ident (VUnassigned tp, localFun ++ "_" ++ ident ++ "_" ++ show freshV) (snd $ localvars funState2)) }
  where 
     created Nothing  = Nothing
     created (Just _) = error $ "Variable " ++ ident ++ " already created"

updateValue :: Ident -> Value -> Eval ()
updateValue ident value = 
  do  funState <- get
      s <- evalMaybe ("Variable " ++ ident ++ " does not exist") $ updlocal funState
      put s
  where
      updlocal m =
          case M.lookup ident (snd $ localvars m) of
            Nothing -> updglobal m
            Just (fVal,fIdent) -> Just $ m { localvars = (fst $ localvars m, M.insert ident (genCast (valueToType fVal) value, fIdent) (snd $ localvars m)) }
      updglobal m =
          case M.lookup ident (snd $ globalvars m) of
            Nothing -> Nothing
            Just (fVal,fIdent) -> Just $ m { globalvars = (fst $ globalvars m, M.insert ident (genCast (valueToType fVal) value, fIdent) (snd $ globalvars m)) }

updateArray :: Ident -> Value -> Value -> Eval ()
updateArray ident valueI value  = 
  do  funState <- get
      s <- evalMaybe ("Variable " ++ ident ++ " does not exist") $ updlocal funState
      put s
  where
      updlocal m =
          case M.lookup ident (snd $ localvars m) of
            Nothing -> updglobal m
            Just (fVal,fIdent) -> Just $ m { localvars = (fst $ localvars m, M.insert ident (fVal // (valueI, value), fIdent) (snd $ localvars m)) }
      updglobal m =
          case M.lookup ident (snd $ globalvars m) of
            Nothing -> Nothing
            Just (fVal,fIdent) -> Just $ m { globalvars = (fst $ globalvars m, M.insert ident (fVal // (valueI, value), fIdent) (snd $ globalvars m)) }

pushLocal :: Eval()
pushLocal = 
  do  funState <- get
      let l = localvars funState
          s = stack funState
      put funState { localvars = ("",M.empty), stack = (l:s)}


popLocal :: Eval()
popLocal =
  do funState <- get
     let s = stack funState
     if   s == []
       then error "There are not function to return from."
       else put funState { localvars = head s, stack = tail s}

-------------------------------------------------------------------------------
-- ** Environment for functions
-------------------------------------------------------------------------------


type FunMap = M.Map Ident Func
data FunEnv = FunEnv { funMap    :: FunMap
                     , size      :: VarSize
                     , repairVar :: Maybe Ident
                     , settings  :: Settings
                     }
              deriving (Eq, Show)

-- |Lookup a function in the function environment
lookupFunction :: Ident -> Eval Func
lookupFunction ident = 
  do  funcEnv <- ask
      evalMaybe ("Function "++ ident ++" not found") $ M.lookup ident $ funMap funcEnv

getValSize :: Eval VarSize
getValSize =
  do  funcEnv <- ask 
      return $ size funcEnv

getRepairVariable :: Eval (Maybe Ident)
getRepairVariable =
  do  funcEnv <- ask 
      return $ repairVar funcEnv

isPerformingSSA :: Eval(Bool)
isPerformingSSA = return . procSSA . settings =<< ask
