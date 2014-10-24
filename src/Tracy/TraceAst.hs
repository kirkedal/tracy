-----------------------------------------------------------------------------
--
-- Module      :  TraceAst
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Abstract syntax tree trace
--
-----------------------------------------------------------------------------

module Tracy.TraceAst where

import Tracy.Ast
import Tracy.AstPretty
import Tracy.Types
import Data.List (intercalate)

type Trace      = [Action]

-------------------------------------------------------------------------------
-- ** Output for showing programs and values
-------------------------------------------------------------------------------
produceOutput :: Settings -> Trace -> String
produceOutput set trace = intercalate "\n" $ map (outputTrace outputForm) $ filterDebug trace
  where 
    filterDebug t
      | debugMode set = t
      | otherwise     = filter notDebug t
    notDebug (Debug _) = False
    notDebug _         = True
    outputForm
      | (outputFormat set) == Z3Int = outputZ3Int
      | (outputFormat set) == Z3Bit = pretty
      | (outputFormat set) == Ccode = prettyC

outputTrace :: (Expr -> String) -> Action -> String
outputTrace _   (Decl   _ typ ident)  = "DECL " ++ pretty typ ++ " " ++ ident
outputTrace fun (Assume _ expr)       = "ASSUME " ++ fun expr
outputTrace fun (Assign _ ident expr) = ident ++ " := " ++ fun expr
outputTrace fun (Assert _ expr)       = "ASSERT " ++ fun expr
--output (Error  action)       = "error..."
outputTrace fun (Error  action)       = "DEBUG:ERROR " ++ outputTrace fun action
outputTrace _   (Debug  string)       = "DEBUG " ++ string
outputTrace _   (Begin  string)       = "BEGIN(" ++ string ++ ")"
outputTrace _   (End    string)       = "END(" ++ string ++ ")"

class Output a where
  outputZ3Int :: a -> String

instance Output Expr where
  outputZ3Int (VarExpr ident) = ident
  outputZ3Int (HexExpr int) = show int
  outputZ3Int (ArrayExpr ident expr) = ident ++ "[" ++ outputZ3Int expr ++ "]"
  outputZ3Int (TrueExpr) = "true"
  outputZ3Int (FalseExpr) = "false"
  outputZ3Int (ConstExpr _ c) = show c
  outputZ3Int (ErrorExpr _c) = ""
  outputZ3Int (UnOpExpr unOp expr) = "(" ++ outputZ3Int unOp ++ " " ++ outputZ3Int expr ++ ")"
  outputZ3Int (BinOpExpr expr1 binOp expr2) =  
    if   elem binOp [Neq]
    then outputZ3Int $ UnOpExpr Not $ BinOpExpr expr1 Eq expr2
    else "(" ++ outputZ3Int binOp ++ " " ++ outputZ3Int expr1 ++ " " ++ outputZ3Int expr2 ++ ")"
  outputZ3Int (FunCallExpr ident exprs) = error "Function calls are unrolled in traces."
  outputZ3Int (CondExpr exprC expr1 expr2) = "(ite" ++ outputZ3Int exprC ++ " " ++ outputZ3Int expr1 ++ " " ++ outputZ3Int expr2 ++ ")"

instance Output BinOp where 
  outputZ3Int Plus   = "+"
  outputZ3Int Minus  = "-"
  outputZ3Int Mult   = "*"
  outputZ3Int Div    = "/"
  outputZ3Int And    = "and"
  outputZ3Int Or     = "or"
  outputZ3Int Eq     = "="
  outputZ3Int Neq    = error "Not Equal is not defined in Z3 int."
  outputZ3Int ULeq   = "<="
  outputZ3Int UGeq   = ">="
  outputZ3Int ULth   = "<"
  outputZ3Int UGth   = ">"
  outputZ3Int Leq    = "<="
  outputZ3Int Geq    = ">="
  outputZ3Int Lth    = "<"
  outputZ3Int Gth    = ">"
  outputZ3Int BAnd   = "&"
  outputZ3Int BOr    = "|"
  outputZ3Int BXor   = "^"
  outputZ3Int SLeft  = "<<"
  outputZ3Int SRight = ">>"
  outputZ3Int BitIdx = "BIT"

instance Output UnOp where
  outputZ3Int Not        = "not"
  outputZ3Int BNot       = "not"
  outputZ3Int Neg        = "-"
  outputZ3Int ToInt      = "(int)"
  outputZ3Int ToUnsigned = "(unsigned)"
