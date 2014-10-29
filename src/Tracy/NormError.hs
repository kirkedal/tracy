-----------------------------------------------------------------------------
--
-- Module      :  PreParse
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Preparser for a small C syntax
--
-----------------------------------------------------------------------------

module Tracy.NormError (normError) where

import Tracy.Ast
import Control.Monad.State

normError :: Program -> Program
normError (globals, funs) = (globals, afterBoolCorr)
  where
    afterFresh = map ppFreshFunc funs
    afterBoolCorr = map normBoolFunc afterFresh

normBoolCond :: Expr -> Expr
normBoolCond e = normBoolExpr e BoolExpr

normBoolFunc :: Func -> Func
normBoolFunc fun = fun { body = b }
  where
    b = normBoolStmts $ body fun

data ExprType = BoolExpr | IntExpr

normBoolStmts :: [Stmt] -> [Stmt]
normBoolStmts = map normBoolStmt

normBoolStmt :: Stmt -> Stmt
normBoolStmt (Cond expr stmt1 stmt2 pos) = Cond (normBoolExpr expr BoolExpr) (normBoolStmts stmt1) (normBoolStmts stmt2) pos
--normBoolStmt (Expression expr) = Expression $ normBoolExpr IntExpr expr
--normBoolStmt (Assign ident expr) = Assign ident $ normBoolExpr IntExpr expr
--              | AssignArray Ident Expr Expr                       -- ^ Assignment to a wire (range)
normBoolStmt (While expr stmt pos) = While (normBoolExpr expr BoolExpr) (normBoolStmts stmt) pos
--              | Assert Int Expr
normBoolStmt (BlockStmt bname stmt posf post) = BlockStmt bname (normBoolStmts stmt) posf post
normBoolStmt s = s

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

--normBoolCorrect TargetType CurrentType Expr
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

------ FRESH

ppFreshFunc :: Func -> Func
ppFreshFunc func = func { body = b }
  where
    run = 
      do  s <- ppStmts $ body func
          t <- extractTypes
          return $ t ++ s
    b = evalState run emptyFN


ppStmts :: [Stmt] -> FreshNames [Stmt]
ppStmts stmts = liftM concat $ mapM ppStmt stmts

ppStmt :: Stmt -> FreshNames [Stmt]
ppStmt (Expression expr pos) = 
  do setSourcePos pos 
     e <- ppExpr expr
     err <- extractErrors 
     return $ err ++ [Expression e pos]
ppStmt (AssignVar ident expr pos) = 
  do setSourcePos pos 
     e   <- ppExpr expr 
     err <- extractErrors 
     return $ err ++ [AssignVar ident e pos]
ppStmt (AssignArray ident expr1 expr2 pos) = 
  do setSourcePos pos 
     e1 <- ppExpr expr1
     e2 <- ppExpr expr2
     err <- extractErrors 
     return $ err ++ [AssignArray ident e1 e2 pos]
ppStmt (Return expr pos) =
  do setSourcePos pos 
     e   <- ppExpr expr
     err <- extractErrors 
     return $ err ++ [Return e pos]
ppStmt (Cond expr stmt1 stmt2 pos) =
  do setSourcePos pos 
     e  <- ppExpr expr
     err <- extractErrors 
     s1 <- ppStmts stmt1
     s2 <- ppStmts stmt2
     return $ err ++ [Cond e s1 s2 pos]
ppStmt (While expr stmt pos) =
  do setSourcePos pos 
     e  <- ppExpr expr
     err <- extractErrors 
     s <- ppStmts stmt
     return $ err ++ [While e s pos]
ppStmt (AssertStmt int expr pos) =
  do setSourcePos pos 
     e   <- ppExpr expr
     err <- extractErrors 
     return $ err ++ [AssertStmt int e pos]
ppStmt s = return [s]

extractErrors :: FreshNames [Stmt]
extractErrors =
  do  fnState <- get
      put $ fnState {assignList = []}
      case assignList fnState of
        []        -> return []
        _otherwise -> return $ [BlockStmt "error" (map (\(i,e,p) -> AssignVar i e p) (assignList fnState)) (currentPos fnState) (currentPos fnState)]

extractTypes :: FreshNames [Stmt]
extractTypes =
  do  fnState <- get
      return $ map (\(i,t,p) -> DefVar i t p) $ declList fnState

ppExpr :: Expr -> FreshNames Expr
ppExpr (ErrorExpr expr)       = liftM VarExpr $ getFreshId expr $ TVal TInt Unsigned
ppExpr (UnOpExpr unOp expr)   = liftM (UnOpExpr unOp) $ ppExpr expr
ppExpr (ArrayExpr ident expr) = liftM (ArrayExpr ident) $ ppExpr expr
ppExpr (BinOpExpr expr1 binOp expr2) = 
  do  e1 <- ppExpr expr1
      e2 <- ppExpr expr2
      return $ BinOpExpr e1 binOp e2
ppExpr (FunCallExpr ident exprs) = liftM (FunCallExpr ident) $ mapM ppExpr exprs
ppExpr (CondExpr expr1 expr2 expr3) =
  do  e1 <- ppExpr expr1
      e2 <- ppExpr expr2
      e3 <- ppExpr expr3
      return $ CondExpr e1 e2 e3
ppExpr e = return e


-------------------------------------------------------------------------------
-- ** State monad for creating new names.
-------------------------------------------------------------------------------

type FreshNames a = State (FN) a

data FN = FN { varInt     :: Int
             , assignList :: [(Ident, Expr, SourcePos)]
             , declList   :: [(Ident, Type, SourcePos)]
             , currentPos :: SourcePos
             }
             deriving (Eq, Show)

-- |Gives a clean FreshNames state
emptyFN :: FN
emptyFN = FN { varInt     = 1
             , assignList = []
             , declList   = []
             , currentPos = initSourcePos
             }

setSourcePos :: SourcePos -> FreshNames ()
setSourcePos pos =
  do fnState <- get
     put $ fnState {currentPos = pos}

-- |Returns the fresh name of a given identifier
getFreshId :: Expr -> Type -> FreshNames String
getFreshId expr typ = 
  do fnState <- get 
     let var = "_var_" ++ (show $ varInt fnState)
         pos = currentPos fnState
     put $ fnState{varInt = (varInt fnState) + 1
                  , assignList = (var, expr, pos):(assignList fnState)
                  , declList =  (var, typ, pos):(declList fnState)}
     return var




