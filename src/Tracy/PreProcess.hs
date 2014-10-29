-----------------------------------------------------------------------------
--
-- Module      :  PreProcess
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Preprocessor for a small C syntax
--
-----------------------------------------------------------------------------

module Tracy.PreProcess (preprocess, normBoolCond) where

import Tracy.Ast
import Tracy.Types

import Control.Monad.State
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.List (nub, (\\), intersect, reverse)

preprocess :: Settings -> Program -> Program
preprocess set p@(globals, funs) 
  | proc3AC set = (globals, map ppFuncTAC funs)
  | otherwise   = p

-------------------------------------------------------------------------------
-- ** Normalisation of Booleans in conditions
-------------------------------------------------------------------------------

normBoolCond :: Expr -> Expr
normBoolCond e = normBoolExpr e BoolExpr

data ExprType = BoolExpr | IntExpr

normBoolExpr :: Expr -> ExprType -> Expr
normBoolExpr e@(UnOpExpr unOp expr) typ = normBoolCorrect typ thisType $ UnOpExpr unOp exprCorr
  where
    (thisType, subType) = normBoolExprType e
    exprCorr = normBoolExpr expr subType
normBoolExpr e@(BinOpExpr expr1 binOp expr2) typ = normBoolCorrect typ thisType $ BinOpExpr exprCorr1 binOp exprCorr2
  where
    (thisType, subType)  = normBoolExprType e
    exprCorr1 = normBoolExpr expr1 subType
    exprCorr2 = normBoolExpr expr2 subType
normBoolExpr (ErrorExpr expr) typ = ErrorExpr $ normBoolExpr expr typ
normBoolExpr (CondExpr exprC expr1 expr2) typ = 
  CondExpr (normBoolExpr exprC BoolExpr) (normBoolExpr expr1 typ) (normBoolExpr expr2 typ)
normBoolExpr expr typ = normBoolCorrect typ thisType expr
  where
    (thisType, _) = normBoolExprType expr
    
normBoolCorrect :: ExprType -> ExprType -> Expr -> Expr
normBoolCorrect BoolExpr BoolExpr e = e
normBoolCorrect BoolExpr IntExpr  e = BinOpExpr e Neq (ConstExpr Signed 0)
normBoolCorrect IntExpr  BoolExpr e = CondExpr e (ConstExpr Signed 1) (ConstExpr Signed 0)
normBoolCorrect IntExpr  IntExpr  e = e

normBoolExprType :: Expr -> (ExprType, ExprType)
normBoolExprType TrueExpr  = (BoolExpr, BoolExpr)
normBoolExprType FalseExpr = (BoolExpr, BoolExpr)
normBoolExprType (BinOpExpr _ binOp _ ) 
  | elem binOp [ULeq, UGeq, ULth, UGth, Leq, Geq, Lth, Gth, Eq, Neq] = (BoolExpr, IntExpr)
  | elem binOp [And, Or] = (BoolExpr, BoolExpr)
  | otherwise     = (IntExpr, IntExpr)
normBoolExprType (UnOpExpr unOp _ ) 
  | elem unOp [Not] = (BoolExpr, BoolExpr)
  | otherwise     = (IntExpr, IntExpr)
normBoolExprType _ = (IntExpr, IntExpr)

-------------------------------------------------------------------------------
-- ** Three address code
-------------------------------------------------------------------------------

ppFuncTAC :: Func -> Func
ppFuncTAC func = func { body = b }
  where
    run = 
      do s <- ppStmts $ body func
         t <- extractTypes
         return $ t ++ s
    b = evalState run $ updFNWithArgs emptyFN $ args func

ppStmts :: [Stmt] -> FreshNames [Stmt]
ppStmts stmts = liftM concat $ mapM ppStmt stmts

ppStmt :: Stmt -> FreshNames [Stmt]
ppStmt (DefVar ident typ pos) = 
  do setSourcePos pos 
     insertFreshFromIdent ident typ
     return []
ppStmt (Expression expr pos) = 
  do setSourcePos pos 
     e  <- ppExpr Top expr
     es <- extractExprs 
     return $ es ++ [Expression e pos]
ppStmt (AssignVar ident expr pos) = 
  do setSourcePos pos 
     e  <- ppExpr Top expr 
     es <- extractExprs 
     i  <- getFreshFromIdent ident
     return $ es ++ [AssignVar i e pos]
ppStmt (AssignArray ident expr1 expr2 pos) = 
  do setSourcePos pos 
     e1 <- ppExpr Top expr1
     e2 <- ppExpr Top expr2
     es <- extractExprs 
     return $ es ++ [AssignArray ident e1 e2 pos]
ppStmt (Return expr pos) =
  do setSourcePos pos 
     e  <- ppExpr Top expr
     es <- extractExprs 
     return $ es ++ [Return e pos]
ppStmt (Cond expr stmt1 stmt2 pos) =
  do setSourcePos pos 
     e  <- ppExpr Top expr
     es <- extractExprs
     s1 <- ppStmts stmt1
     s2 <- ppStmts stmt2
     return $ es ++ [Cond e s1 s2 pos]
ppStmt (While expr stmt pos) =
  do setSourcePos pos 
     e  <- ppExpr Top expr
     es <- extractExprs 
     s  <- ppStmts stmt
     return $ es ++ [While e (s ++ es) pos]
ppStmt (AssertStmt int expr pos) =
  do setSourcePos pos 
     e  <- ppExpr Top expr
     es <- extractExprs 
     return $ es ++ [AssertStmt int e pos]
ppStmt (BlockStmt ident stmts posf post) =
  do setSourcePos posf 
     ss <- ppStmts stmts
     return $ [BlockStmt ident ss posf post]
ppStmt s = return [s]


extractExprs :: FreshNames [Stmt]
extractExprs =
  do  fnState <- get
      put $ fnState {assignList = []}
      return $ reverse $ map (\(i,e,p) -> AssignVar i e p) $ assignList fnState

extractTypes :: FreshNames [Stmt]
extractTypes =
  do  fnState <- get
      put $ fnState {declList = []}
      return $ reverse $ map (\(i,t,p) -> DefVar i t p) $ declList fnState

data Place = Top | Rest


-- No treatment of Boolean exprs
ppExpr :: Place -> Expr -> FreshNames Expr
ppExpr _ (ErrorExpr expr) = error "Error should have been transformed to statements."
ppExpr _ (ArrayExpr ident expr) = 
  do  e <- ppExpr Rest expr
      return $ ArrayExpr ident e
ppExpr _ (IncDecExpr IncPre ident) = 
  do i    <- lookupVar ident
     inew <- getFreshVarFromIdent ident (BinOpExpr (VarExpr i) Plus (ConstExpr Signed 1))
     return $ VarExpr inew
ppExpr _ (IncDecExpr IncPost ident) = 
  do i    <- lookupVar ident
     inew <- getFreshVarFromIdent ident (BinOpExpr (VarExpr i) Plus (ConstExpr Signed 1))
     return $ VarExpr i
ppExpr _ (IncDecExpr DecPre ident) = 
  do i    <- lookupVar ident
     inew <- getFreshVarFromIdent ident (BinOpExpr (VarExpr i) Minus (ConstExpr Signed 1))
     return $ VarExpr inew
ppExpr _ (IncDecExpr DecPost ident) = 
  do i    <- lookupVar ident
     inew <- getFreshVarFromIdent ident (BinOpExpr (VarExpr i) Minus (ConstExpr Signed 1))
     return $ VarExpr i
ppExpr Top (UnOpExpr unOp expr) = 
  do  e <- ppExpr Rest expr
      return $ UnOpExpr unOp e
ppExpr Rest (UnOpExpr unOp expr) = 
  do  e <- ppExpr Rest expr
      v <- getFreshVar (UnOpExpr unOp e) (TVal TInt Signed)
      return $ VarExpr v
ppExpr Top (BinOpExpr expr1 binOp expr2) = 
  do  e1 <- ppExpr Rest expr1
      e2 <- ppExpr Rest expr2
      return $ BinOpExpr e1 binOp e2
ppExpr Rest (BinOpExpr expr1 binOp expr2) =
  do  e1 <- ppExpr Rest expr1
      e2 <- ppExpr Rest expr2
      v  <- getFreshVar (BinOpExpr e1 binOp e2) (TVal TInt Signed)
      return $ VarExpr v
ppExpr Top (CondExpr exprB expr1 expr2) = 
  do  eb <- ppExpr Rest exprB
      e1 <- ppExpr Rest expr1
      e2 <- ppExpr Rest expr2
      return $ CondExpr eb e1 e2
ppExpr Rest (CondExpr exprB expr1 expr2) = 
  do  eb <- ppExpr Rest exprB
      e1 <- ppExpr Rest expr1
      e2 <- ppExpr Rest expr2
      v  <- getFreshVar (CondExpr eb e1 e2) (TVal TInt Signed)
      return $ VarExpr v
ppExpr Top (FunCallExpr ident exprs) = 
  do  es <- mapM (ppExpr Rest) exprs
      return $ FunCallExpr ident es
ppExpr Rest (FunCallExpr ident exprs) = 
  do  es <- mapM (ppExpr Rest) exprs
      v <- getFreshVar (FunCallExpr ident es) (TVal TInt Signed)
      return $ VarExpr v
ppExpr _ (VarExpr ident) = 
  do v <- lookupVar ident
     return $ VarExpr v
ppExpr _ e = return e


assignedVarsStmts :: [Stmt] -> [Ident]
assignedVarsStmts stmts = nub $ concatMap assignedVarsStmt stmts


assignedVarsStmt :: Stmt -> [Ident]
assignedVarsStmt (AssignVar ident _ _) = [ident]
assignedVarsStmt (AssignArray ident _ _ _) = [ident]
assignedVarsStmt (Cond _ stmt1 stmt2 _) = assignedVarsStmts $ stmt1 ++ stmt2
assignedVarsStmt (While _ stmts _) = assignedVarsStmts stmts
assignedVarsStmt (BlockStmt _ stmts _ _) = assignedVarsStmts stmts
assignedVarsStmt _ = []

-------------------------------------------------------------------------------
-- ** State monad for creating new names.
-------------------------------------------------------------------------------

type FreshNames a = State (FN) a

data FN = FN { varInt     :: Int
             , identMap   :: Map.Map Ident (Ident, Type)
             , assignList :: [(Ident, Expr, SourcePos)]
             , declList   :: [(Ident, Type, SourcePos)]
             , currentPos :: SourcePos
             }
             deriving (Eq, Show)


setSourcePos :: SourcePos -> FreshNames ()
setSourcePos pos =
  do fnState <- get
     put $ fnState {currentPos = pos}

-- |Gives a clean FreshNames state
emptyFN :: FN
emptyFN = FN { varInt     = 1
             , identMap   = Map.empty
             , assignList = []
             , declList   = []
             , currentPos = initSourcePos
             }


updFNWithArgs :: FN -> [Stmt] -> FN
updFNWithArgs = foldl updFNWithArg

updFNWithArg :: FN -> Stmt -> FN
updFNWithArg fn (DefVar ident typ _) = fn {identMap = Map.insert ident (ident, typ) (identMap fn)}


-- |Returns the fresh name of a given identifier
getFreshVar :: Expr -> Type -> FreshNames Ident
getFreshVar expr typ  = 
  do  fnState <- get
      let varI = varInt fnState
          var = "_var_" ++ show (varInt fnState)
          pos = currentPos fnState
      put $ fnState {varInt = varI + 1, assignList = (var, expr, pos):(assignList fnState), declList = (var, typ, pos):(declList fnState)}
      return var

getFreshFromIdent :: Ident -> FreshNames Ident
getFreshFromIdent ident  = 
  do  fnState <- get
      let varI = varInt fnState
          var = ident
          (_,typ) = fromMaybe (error "Variable not found.") $  Map.lookup ident $ identMap fnState
      put $ fnState {varInt = varI, identMap = Map.insert ident (var, typ) (identMap fnState)}
      return var

getFreshVarFromIdent :: Ident -> Expr -> FreshNames Ident
getFreshVarFromIdent ident expr = 
  do  fnState <- get
      let varI = varInt fnState
          var = ident
          (_,typ) = fromMaybe (error $ "Variable "++ ident ++" not found.") $  Map.lookup ident $ identMap fnState
          pos = currentPos fnState
      put $ fnState { assignList = (var, expr, pos):(assignList fnState)
                    , identMap = Map.insert ident (var, typ) (identMap fnState)}
      return var

insertFreshFromIdent :: Ident -> Type -> FreshNames Ident
insertFreshFromIdent ident typ= 
  do  fnState <- get
      let var = ident
          pos = currentPos fnState      
      put $ fnState {declList = (var, typ, pos):(declList fnState), identMap = Map.insert ident (var, typ) (identMap fnState)}
      return var

lookupVar :: Ident -> FreshNames Ident
lookupVar ident = 
  do fnState <- get
     errorMaybe $ Map.lookup ident $ identMap fnState
  where
    errorMaybe Nothing  = return ident
    errorMaybe (Just (s,_)) = return s
