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

import qualified Tracy.Ast as Ast
import Tracy.AstPretty
import Tracy.Types
import Data.List (intercalate)

type Trace      = [Action]

-- Error numbers are not handled yet
data Action     = Decl   ErrorNumber Ast.Type Ast.Ident
                | Assume ErrorNumber Ast.Expr
                | Assign ErrorNumber Ast.Ident Ast.Expr
                | Assert ErrorNumber Ast.Expr
                | Error  Action
                | Debug  String
                | Begin  String
                | End    String
                deriving (Eq, Show)

type ErrorNumber = String

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

outputTrace :: (Ast.Expr -> String) -> Action -> String
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

instance Output Ast.Expr where
  outputZ3Int (Ast.VarExpr ident) = ident
  outputZ3Int (Ast.HexExpr int) = show int
  outputZ3Int (Ast.ArrayExpr ident expr) = ident ++ "[" ++ outputZ3Int expr ++ "]"
  outputZ3Int (Ast.TrueExpr) = "true"
  outputZ3Int (Ast.FalseExpr) = "false"
  outputZ3Int (Ast.ConstExpr _ c) = show c
  outputZ3Int (Ast.ErrorExpr _c) = ""
  outputZ3Int (Ast.UnOpExpr unOp expr) = "(" ++ outputZ3Int unOp ++ " " ++ outputZ3Int expr ++ ")"
  outputZ3Int (Ast.BinOpExpr expr1 binOp expr2) =  
    if   elem binOp [Ast.Neq]
    then outputZ3Int $ Ast.UnOpExpr Ast.Not $ Ast.BinOpExpr expr1 Ast.Eq expr2
    else "(" ++ outputZ3Int binOp ++ " " ++ outputZ3Int expr1 ++ " " ++ outputZ3Int expr2 ++ ")"
  outputZ3Int (Ast.FunCallExpr ident exprs) = error "Function calls are unrolled in traces."
  outputZ3Int (Ast.CondExpr exprC expr1 expr2) = "(ite" ++ outputZ3Int exprC ++ " " ++ outputZ3Int expr1 ++ " " ++ outputZ3Int expr2 ++ ")"

instance Output Ast.BinOp where 
  outputZ3Int Ast.Plus   = "+"
  outputZ3Int Ast.Minus  = "-"
  outputZ3Int Ast.Mult   = "*"
  outputZ3Int Ast.Div    = "/"
  outputZ3Int Ast.And    = "and"
  outputZ3Int Ast.Or     = "or"
  outputZ3Int Ast.Eq     = "="
  outputZ3Int Ast.Neq    = error "Not Equal is not defined in Z3 int."
  outputZ3Int Ast.ULeq   = "<="
  outputZ3Int Ast.UGeq   = ">="
  outputZ3Int Ast.ULth   = "<"
  outputZ3Int Ast.UGth   = ">"
  outputZ3Int Ast.Leq    = "<="
  outputZ3Int Ast.Geq    = ">="
  outputZ3Int Ast.Lth    = "<"
  outputZ3Int Ast.Gth    = ">"
  outputZ3Int Ast.BAnd   = "&"
  outputZ3Int Ast.BOr    = "|"
  outputZ3Int Ast.BXor   = "^"
  outputZ3Int Ast.SLeft  = "<<"
  outputZ3Int Ast.SRight = ">>"
  outputZ3Int Ast.BitIdx = "BIT"

instance Output Ast.UnOp where
  outputZ3Int Ast.Not        = "not"
  outputZ3Int Ast.BNot       = "not"
  outputZ3Int Ast.Neg        = "-"
  outputZ3Int Ast.ToInt      = "(int)"
  outputZ3Int Ast.ToUnsigned = "(unsigned)"
