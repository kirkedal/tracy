-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Implementation of the tracer
--
-----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}

module Tracy.Trace where

import Tracy.Ast
import Tracy.AstExec hiding (not, and, or)
import qualified Tracy.AstExec as Ast (not, and, or)
import Tracy.AstPretty
import Tracy.Types hiding (repairVar)
import qualified Tracy.Types as T
import qualified Tracy.TraceAst as TA
import Tracy.Eval

import BitVector (integerWidth, bitVec)

import System.Random
import Control.Monad
import Control.Exception
import Data.Typeable
import System.Exit
import Text.Printf (printf)
import Data.Maybe (fromMaybe)

import qualified Data.Map as M

interp :: Program -> Settings ->  Expr -> Expr -> IO EvalTrace
interp (globals, funclist) set preExpr postExpr = 
  do vals <- generateInput values funEnv ident
     res  <- catch 
                (runEval (interpProgram globals ident vals preExpr postExpr) emptyState funEnv)
                traceExceptionHandler     
     return $ snd res
  where
    ident  = function set
    values = assignment set
    repVar = T.repairVar set
    valsize = sint $ varSize set
    funEnv = FunEnv { funMap = M.fromList $ map (\x -> (funcname x, x)) funclist
                    , size = VarSize {sint = valsize, sshort = valsize `div` 2, schar = 8, slong = valsize * 2}
                    , repairVar = repVar, settings = set }

-------------------------------------------------------------------------------
-- ** Generating Values
-------------------------------------------------------------------------------

generateInput :: InputType -> FunEnv -> Ident -> IO [Value]
generateInput (InputValues string values) _ _ = return values  -- Check size of values
generateInput InputRandom f i = genInput f i randomValue
generateInput InputZero   f i = genInput f i zeroValue

genInput :: FunEnv -> Ident -> (Type -> IO Value) -> IO [Value]
genInput funEnv ident generator = 
  mapM generator $ map argsToType $ args topFun
  where
    topFun = fromMaybe ("Function "++ ident ++" not found") $ M.lookup ident $ funMap funEnv
    argsToType  (DefVar _ typ _) = typ
    fromMaybe e Nothing = error e
    fromMaybe _ (Just a) = a

randomInt :: Int -> Int -> IO Int
randomInt l d = getStdRandom (randomR (l, d))

randomValue :: Type -> IO Value
randomValue (TVal t p) = 
  do i <- randomInt 0 100000
     return $ VVal t p $ bitVec 32 i

zeroValue :: Type -> IO Value
zeroValue (TVal t p) = return $ VVal t p $ bitVec 32 0
zeroValue (TArray (TVal t p) (Just s)) = return $ VArray t p $ emptyArray s

-------------------------------------------------------------------------------
-- ** Exceptions for tracing
-------------------------------------------------------------------------------

data TraceException = ConstantSize_4 
                    | ConstantSize_8 
                    | ConstantSize_16 
                    | ConstantSize_32 
    deriving (Show, Typeable)

instance Exception TraceException

traceExceptionHandler :: TraceException -> IO a
traceExceptionHandler ConstantSize_4  = (putStrLn "Constant size exceeds bit-width. Must be 4 bit.")  >> exitWith (ExitFailure 31)
traceExceptionHandler ConstantSize_8  = (putStrLn "Constant size exceeds bit-width. Must be 8 bit.")  >> exitWith (ExitFailure 32)
traceExceptionHandler ConstantSize_16 = (putStrLn "Constant size exceeds bit-width. Must be 16 bit.") >> exitWith (ExitFailure 33)
traceExceptionHandler ConstantSize_32 = (putStrLn "Constant size exceeds bit-width. Must be 32 bit.") >> exitWith (ExitFailure 34)
--traceExceptionHandler _ = exitWith $ ExitFailure 32

-------------------------------------------------------------------------------
-- ** Interpreter for tracing
-------------------------------------------------------------------------------

interpProgram :: [Stmt] -> Ident -> [Value] -> Expr -> Expr -> Eval()
interpProgram defs topFun values preExpr postExpr = 
  do  setTopFun topFun
      setLocalFun topFun
      fun <- lookupFunction topFun
      case getFirstArgPos $ args fun of
        Nothing  -> tell []
        Just pos ->
          do tell [Begin pos "arguments"]
             mapM_ interpDefValue (zip (args fun) values)
             tell [End pos "arguments"]
      mapM_ interpDefGlobal defs
      interpStmts (body fun)
      (_,preExprUpd) <- interpExpr preExpr
      tracePrePost "precondition" preExprUpd
      (_,postExprUpd) <- interpExpr postExpr
      tracePrePost "postcondition" postExprUpd
      return ()
  where 
    getFirstArgPos [] = Nothing
    getFirstArgPos ((DefVar _ _ p):ls) = Just p

interpDefGlobal :: Stmt -> Eval()
interpDefGlobal (DefVar ident t@(TArray typ (Just s)) _) = 
  initGlobalArray ident typ $ emptyArray s 
interpDefGlobal (DefVar ident tp _) = 
  initGlobalValue ident tp
-- interpDefGlobal _ = undefined

interpFunc :: Func -> [Value] -> Eval(Value)
interpFunc func values = 
  do  setLocalFun $ funcname func
      mapM_ interpDefValue (zip (args func) values)
      e <- getCallExprs
      mapM_ (uncurry traceAssign) $ zip (map fromDefVar $ args func) e
      interpStmts (body func)
  where 
    fromDefVar (DefVar i _ _) = i
    -- fromDefVar (_rest)      = undefined

interpDefValue :: (Stmt, Value) -> Eval()
interpDefValue ((DefVar ident tp pos), val) = 
  do setSourcePos pos
     initValue ident tp
     updateValue ident val
     traceType ident
-- interpDefValue (_rest) = undefined


interpDef :: Stmt -> Eval()
interpDef (DefVar ident tp pos) = 
  do setSourcePos pos
     initValue ident tp
     traceType ident
-- interpDef (_rest) = undefined

interpStmts :: [Stmt] -> Eval(Value)
interpStmts []                = return VVoid
interpStmts (stmt:stmts)      = 
  do v <- interpStmt stmt
     b <- isSetReturnExpr
     if   b
       then return v
       else interpStmts stmts

interpStmt :: Stmt -> Eval(Value)
interpStmt (DefVar ident t@(TArray typ (Just s)) pos) = 
  do setSourcePos pos
     initArray ident typ (emptyArray s) 
     traceType ident 
     return VVoid
interpStmt (DefVar ident tp pos)   = 
  do setSourcePos pos 
     initValue ident tp 
     traceType ident
     return VVoid
interpStmt (AssignVar ident expr pos) = 
  do setSourcePos pos
     (val, e) <- interpExpr expr 
     update val 
     updateValue ident val
     traceAssign ident e
     return VVoid
  where 
    update val = 
      do b <- isPerformingSSA
         if b then initValue ident (valueToType val) else return ()
interpStmt (Expression expr pos) = 
  do setSourcePos pos
     (val, e) <- interpExpr expr 
     return val
interpStmt (AssignArray ident expr1 expr2 pos) = 
  do setSourcePos pos
     (val1, e1) <- interpExpr expr1
     (val2, e2) <- interpExpr expr2
     traceAssignA ident e1 e2
     traceDebug $ ident ++ "[" ++ pretty val1 ++ "] := " ++ pretty val2
     updateArray ident val1 val2 
     return VVoid
interpStmt (Return expr pos) = 
  do setSourcePos pos
     (v, e) <- interpExpr expr
     setReturnExpr e 
     return v 
interpStmt (Cond expr stmt1 stmt2 pos) = 
  do setSourcePos pos
     (val,exprO) <- interpExpr expr
     traceCond exprO val
     if valueToBool val
       then interpStmts stmt1
       else interpStmts stmt2
interpStmt (While expr stmt pos) = 
  interpStmt (Cond expr (stmt ++ [While expr stmt pos]) [] pos)
interpStmt (AssertStmt int expr pos) = 
  do setSourcePos pos
     (val, e) <- interpExpr expr
     if valueToBool val
       then traceAssert e >> return VVoid
       else traceAssert e >> traceDebug ("Assertion in line " ++ show int ++ " failed.") >> setReturnExpr FalseExpr >> return VVoid
interpStmt (BlockStmt "error" stmts posf post) = 
  do setSourcePos posf
     _t <- isTracing
     setErrorSeen
     tell [Begin posf "error"]
     toggleTracing
     val <- interpStmts stmts
     toggleTracing
     traceRepairVariable 
     tell [End post "error"]
     return val
interpStmt (BlockStmt bname stmts posf post) = 
  do setSourcePos posf
     tell [Begin posf bname]
     val <- interpStmts stmts
     tell [End post bname]
     return val
interpStmt (Spec _ _) = return VVoid

interpExpr :: Expr -> Eval(Value, Expr)
interpExpr e@(HexExpr int) = getValSize >>= \s -> return $ (constToValue Signed s int, e)
interpExpr (VarExpr ident) = 
  do  v <- lookupValue ident
      e <- lookupFName ident
      case v of
        VUnassigned _ -> error $ "Variable [" ++ ident ++ "] is unassigned."
        VPointer _ i  -> (\x -> return (x, VarExpr e)) =<< liftM fst (interpExpr $ VarExpr i)
        otherwise     -> return (v, VarExpr e)
interpExpr e@(ArrayExpr ident expr) = 
  do a <- lookupValue ident
     f <- lookupFName ident
     (v,ex) <- interpExpr expr
     let r = fromMaybe (error $ "Variable [" ++ ident ++ "] is not an array.") $ a ! v
     return (r, ArrayExpr f ex)
interpExpr e@TrueExpr               = getValSize >>= \s -> return $ (boolToValue s True, e)
interpExpr e@FalseExpr              = getValSize >>= \s -> return $ (boolToValue s False, e)
interpExpr e@(StringExpr str)       = return (VString str, e)
interpExpr e@(PReference ident)     = 
  do v <- lookupValue ident
     f <- lookupFName ident
     return $ (VPointer (valueToType v) ident, PReference f)
interpExpr e@(ConstExpr pol int)    = 
  do size <- getValSize 
     checkSize $ sint size
     return $ (constToValue pol size int, e)
  where 
    checkSize s
      | width <= s    = return ()
      | otherwise     = throw sizeToError
    width 
      | pol == Signed = integerWidth $ toInteger int
      | otherwise     = (integerWidth $ toInteger int) - 1
    sizeToError 
      | width <=  4   = ConstantSize_4
      | width <=  8   = ConstantSize_8
      | width <= 16   = ConstantSize_16
      | width <= 32   = ConstantSize_32
      | otherwise = error "Constant exceeds largest bit-width of implementation (32 bits)."
interpExpr (IncDecExpr IncPre ident)  = 
  do p <- getSourcePos
     interpStmt (AssignVar ident (BinOpExpr (VarExpr ident) Plus (ConstExpr Signed 1)) p)
     interpExpr (VarExpr ident)
interpExpr (IncDecExpr IncPost ident) = 
  do p <- getSourcePos
     r <- interpExpr (VarExpr ident)
     interpStmt (AssignVar ident (BinOpExpr (VarExpr ident) Plus (ConstExpr Signed 1)) p)
     return r
interpExpr (IncDecExpr DecPre ident)  = 
  do p <- getSourcePos
     interpStmt (AssignVar ident (BinOpExpr (VarExpr ident) Minus (ConstExpr Signed 1)) p)
     interpExpr (VarExpr ident)
interpExpr (IncDecExpr DecPost ident) = 
  do p <- getSourcePos
     r <- interpExpr (VarExpr ident)
     interpStmt (AssignVar ident (BinOpExpr (VarExpr ident) Minus (ConstExpr Signed 1)) p)
     return r
interpExpr topExpr@(UnOpExpr unOp expr) = 
  do  (v1, e1) <- interpExpr expr
      v  <- interpUnOp unOp v1
      s  <- showState
      let vs = fromMaybe (error $ "Err\n" ++ pretty topExpr ++ "\n" ++ s) v
      return $ (vs, UnOpExpr unOp e1)
interpExpr topExpr@(BinOpExpr expr1 binOp expr2) = 
  do  (v1, e1) <- interpExpr expr1
      (v2, e2) <- interpExpr expr2
      v <- interpBinOp binOp v1 v2
      s <- showState
      let vs = fromMaybe (error $ "Err\n" ++ pretty topExpr ++ "\n" ++ s) v
          op = rightCompOperator binOp v1 v2
      return (vs, BinOpExpr e1 op e2)
interpExpr (CondExpr exprC expr1 expr2) = 
  do  (valC, exprCo) <- interpExpr exprC
      (valT, exprC1) <- interpExpr expr1
      (valF, exprC2) <- interpExpr expr2
      return $ if   valueToBool valC
        then (valT, CondExpr exprCo exprC1 exprC2)
        else (valF, CondExpr exprCo exprC1 exprC2)
interpExpr (FunCallExpr "printf" exprs) = 
  do  evs <- mapM interpExpr exprs
      let   (vals, es) = unzip evs
            s = appPrint $ map pretty es
      traceDebug $ "printing: " ++ s
      return (VVoid, FunCallExpr "printf" es)
  where
    appPrint l = 
      case l of
         [] -> ""
         [e] -> e
         [e1,e2] -> printf e1 e2
         [e1,e2,e3] -> printf e1 e2 e3
         [e1,e2,e3,e4] -> printf e1 e2 e3 e4
interpExpr (FunCallExpr ident exprs) = 
  do  valExprs <- mapM interpExpr exprs
      let (vals, exprO) = unzip valExprs
      fun  <- lookupFunction ident
      setCallExprs exprO
      pushLocal
      res  <- interpFunc fun vals
      popLocal
      retExpr <- getReturnExpr
      unsetReturnExpr
      case retExpr of
        Nothing -> return (res, TrueExpr)
        Just e  -> return (res, e)

rightCompOperator :: BinOp -> Value -> Value -> BinOp
rightCompOperator Lth  (VVal _ Unsigned _) (VVal _ _        _) = ULth
rightCompOperator Lth  (VVal _ _      _)   (VVal _ Unsigned _) = ULth
rightCompOperator Gth  (VVal _ Unsigned _) (VVal _ _        _) = UGth
rightCompOperator Gth  (VVal _ _      _)   (VVal _ Unsigned _) = UGth
rightCompOperator Leq  (VVal _ Unsigned _) (VVal _ _        _) = ULeq
rightCompOperator Leq  (VVal _ _      _)   (VVal _ Unsigned _) = ULeq
rightCompOperator Geq  (VVal _ Unsigned _) (VVal _ _        _) = UGeq
rightCompOperator Geq  (VVal _ _      _)   (VVal _ Unsigned _) = UGeq
rightCompOperator ULth (VVal _ Signed _)   (VVal _ Signed _)   = Lth
rightCompOperator UGth (VVal _ Signed _)   (VVal _ Signed _)   = Gth
rightCompOperator ULeq (VVal _ Signed _)   (VVal _ Signed _)   = Leq
rightCompOperator UGeq (VVal _ Signed _)   (VVal _ Signed _)   = Geq
rightCompOperator op   _                   _                   = op

interpUnOp :: UnOp -> Value -> Eval (Maybe Value)
interpUnOp Not  v       = return $ Ast.not v
interpUnOp Neg  v       = return $ Just $ negate v
interpUnOp BNot v       = return $ not_ v
interpUnOp ToInt v      = return $ toSigned v
interpUnOp ToUnsigned v = return $ toUnsigned v

interpBinOp :: BinOp -> Value -> Value -> Eval (Maybe Value)
interpBinOp op v1@(VUnassigned _) (VUnassigned _) = return $ Just v1
interpBinOp And v1@(VUnassigned _) v2 
  | valueToBool v2 = return $ Just v1
  | otherwise      = return $ Just v2
interpBinOp Or  v1@(VUnassigned _) v2 
  | valueToBool v2 = return $ Just v2
  | otherwise      = return $ Just v1
interpBinOp _   v1@(VUnassigned _) _ = return $ Just v1
interpBinOp And v1 v2@(VUnassigned _)
  | valueToBool v1 = return $ Just v2
  | otherwise      = return $ Just v1
interpBinOp Or  v1 v2@(VUnassigned _)
  | valueToBool v1 = return $ Just v1
  | otherwise      = return $ Just v2
interpBinOp _   _ v2@(VUnassigned _) = return $ Just v2
interpBinOp BOr    v1 v2 = return $ v1 .|. v2
interpBinOp BAnd   v1 v2 = return $ v1 .&. v2
interpBinOp BXor   v1 v2 = return $ v1 .^. v2
interpBinOp SLeft  v1 v2 = return $ v1 <<. v2
interpBinOp SRight v1 v2 = return $ v1 >>. v2
interpBinOp Plus   v1 v2 = return $ Just $ v1 + v2
interpBinOp Minus  v1 v2 = return $ Just $ v1 - v2
interpBinOp Mult   v1 v2 = return $ Just $ v1 * v2
interpBinOp Div    v1 v2 = return $ Just $ v1 `division` v2
interpBinOp And    v1 v2 = return $ Ast.and v1 v2
interpBinOp Or     v1 v2 = return $ Ast.or v1 v2
interpBinOp Eq     v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 == v2
interpBinOp Neq    v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 /= v2
interpBinOp Lth    v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 < v2
interpBinOp Gth    v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 > v2
interpBinOp Leq    v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 <= v2
interpBinOp Geq    v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 >= v2
interpBinOp ULth   v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 < v2
interpBinOp UGth   v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 > v2
interpBinOp ULeq   v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 <= v2
interpBinOp UGeq   v1 v2 = getValSize >>= \s -> return $ Just $ boolToValue s $ v1 >= v2
interpBinOp BitIdx v1 v2 = return $ bitIdx v1 v2
