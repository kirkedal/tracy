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
import qualified Text.Parsec.Pos as P

type Trace      = [Action]

-------------------------------------------------------------------------------
-- ** Output for showing programs and values
-------------------------------------------------------------------------------
produceOutput :: Settings -> Trace -> String
produceOutput set trace = intercalate "\n" $ map (\a -> (showLines a) ++ (outputForm a)) $ filterDebug trace
  where 
    filterDebug t
      | debugMode set = t
      | otherwise     = filter notDebug t
    notDebug (Debug _ _) = False
    notDebug (Error _ _) = False
    notDebug _         = True
    showLines a
      | linenumbers set = (show $ P.sourceLine $ extractPos a) ++ " "
      | otherwise       = ""
    outputForm
      | (outputFormat set) == SMT2_QF_NIA = output_QF_NIA
      | (outputFormat set) == SMT2_QF_BV  = output_QF_BV set
      | (outputFormat set) == Ccode       = outputTraceC

extractPos :: Action -> SourcePos
extractPos (Decl    pos _ _ _)   = pos
extractPos (Assume  pos _ _)     = pos
extractPos (Assign  pos _ _ _)   = pos
extractPos (AssignA pos _ _ _ _) = pos
extractPos (Assert  pos _ _)     = pos
extractPos (Error   pos _)       = pos
extractPos (Debug   pos _)       = pos
extractPos (Begin   pos _)       = pos
extractPos (End     pos _)       = pos


outputTraceC :: Action -> String
outputTraceC (Decl    _ _ typ ident)        = "DECL " ++ (prettyC typ) ++ " " ++ ident
outputTraceC (Assume  _ _ expr)             = "ASSUME " ++ prettyC expr
outputTraceC (Assign  _ _ ident expr)       = ident ++ " := " ++ prettyC expr
outputTraceC (AssignA _ _ ident exprI expr) = ident ++ "[" ++ prettyC exprI ++ "]" ++ " := " ++ prettyC expr
outputTraceC (Assert  _ _ expr)             = "ASSERT " ++ prettyC expr
outputTraceC (Error   _ action)             = "DEBUG:ERROR " ++ outputTraceC action
outputTraceC (Debug   _ string)             = "DEBUG " ++ string
outputTraceC (Begin   _ string)             = "BEGIN(" ++ string ++ ")"
outputTraceC (End     _ string)             = "END(" ++ string ++ ")"

class Output a where
  output_QF_NIA :: a -> String
  output_QF_BV  :: Settings -> a -> String


instance Output Action where
  output_QF_NIA (Decl   _ _ typ ident)         = "DECL " ++ output_QF_NIA typ ++ " " ++ ident
  output_QF_NIA (Assume _ _ expr)              = "ASSUME " ++ output_QF_NIA expr
  output_QF_NIA (Assign _ _ ident expr)        = ident ++ " := " ++ output_QF_NIA expr
  output_QF_NIA (AssignA _ _ ident exprI expr) = ident ++ "[" ++ output_QF_NIA exprI ++ "] := " ++ output_QF_NIA expr
  output_QF_NIA (Assert _ _ expr)              = "ASSERT " ++ output_QF_NIA expr
  output_QF_NIA (Error  _ action)              = "DEBUG:ERROR " ++ output_QF_NIA action
  output_QF_NIA (Debug  _ string)              = "DEBUG " ++ string
  output_QF_NIA (Begin  _ string)              = "BEGIN(" ++ string ++ ")"
  output_QF_NIA (End    _ string)              = "END(" ++ string ++ ")"

  output_QF_BV set (Decl   _ _ typ ident)         = "DECL " ++ output_QF_BV set typ ++ " " ++ ident
  output_QF_BV set (Assume _ _ expr)              = "ASSUME " ++ output_QF_BV set expr
  output_QF_BV set (Assign _ _ ident expr)        = ident ++ " := " ++ output_QF_BV set expr
  output_QF_BV set (AssignA _ _ ident exprI expr) = ident ++ "[" ++ output_QF_BV set exprI ++ "] := " ++ output_QF_BV set expr
  output_QF_BV set (Assert _ _ expr)              = "ASSERT " ++ output_QF_BV set expr
  output_QF_BV set (Error  _ action)              = "DEBUG:ERROR " ++ output_QF_BV set action
  output_QF_BV set (Debug  _ string)              = "DEBUG " ++ string
  output_QF_BV set (Begin  _ string)              = "BEGIN(" ++ string ++ ")"
  output_QF_BV set (End    _ string)              = "END(" ++ string ++ ")"

instance Output Expr where
  output_QF_NIA (VarExpr ident) = ident
  output_QF_NIA (HexExpr int) = show int
  output_QF_NIA (ArrayExpr ident expr) = ident ++ "[" ++ output_QF_NIA expr ++ "]"
  output_QF_NIA (TrueExpr) = "true"
  output_QF_NIA (FalseExpr) = "false"
  output_QF_NIA (ConstExpr _ c) = show c
  output_QF_NIA (ErrorExpr _c) = ""
  output_QF_NIA (UnOpExpr unOp expr) = "(" ++ output_QF_NIA unOp ++ " " ++ output_QF_NIA expr ++ ")"
  output_QF_NIA (BinOpExpr expr1 Neq expr2) = output_QF_NIA $ UnOpExpr Not $ BinOpExpr expr1 Eq expr2
  output_QF_NIA (BinOpExpr expr1 binOp expr2) = "(" ++ output_QF_NIA binOp ++ " " ++ output_QF_NIA expr1 ++ " " ++ output_QF_NIA expr2 ++ ")"
  output_QF_NIA (FunCallExpr ident exprs) = error "Function calls are unrolled in traces."
  output_QF_NIA (CondExpr exprC expr1 expr2) = "(ite" ++ output_QF_NIA exprC ++ " " ++ output_QF_NIA expr1 ++ " " ++ output_QF_NIA expr2 ++ ")"

  output_QF_BV set (VarExpr ident) = ident
  output_QF_BV set (StringExpr str) = str
  output_QF_BV set (HexExpr int) = show int
  output_QF_BV set (ArrayExpr ident expr) = ident ++ "[" ++ output_QF_BV set expr ++ "]"
  output_QF_BV set (TrueExpr) = "ctx.bool_val(true)"
  output_QF_BV set (FalseExpr) = "ctx.bool_val(false)"
  output_QF_BV set (ConstExpr _ c) = "ctx.bv_val(" ++ show c ++ ", bitwidth)"
  output_QF_BV set (ErrorExpr _c) = ""
  output_QF_BV set (UnOpExpr unOp expr) = "(" ++ output_QF_BV set unOp ++ " " ++ output_QF_BV set expr ++ ")"
  output_QF_BV set (BinOpExpr expr1 Neq expr2) = output_QF_BV set $ UnOpExpr Not $ BinOpExpr expr1 Eq expr2
  output_QF_BV set (BinOpExpr expr1 binOp expr2) = 
    if   elem binOp [ULeq, UGeq, ULth, UGth, BitIdx]
    then "(" ++ output_QF_BV set binOp ++ " " ++ output_QF_BV set expr1 ++ " " ++ output_QF_BV set expr2 ++ ")"
    else "(" ++ output_QF_BV set binOp ++ " " ++ output_QF_BV set expr1 ++ " " ++ output_QF_BV set expr2 ++ ")"
  output_QF_BV set (FunCallExpr ident exprs) = ident ++ "(" ++ intercalate ", " (map (output_QF_BV set) exprs) ++ ")"
  output_QF_BV set (CondExpr exprC expr1 expr2) = "(ite" ++ output_QF_BV set exprC ++ ", " ++ output_QF_BV set expr1 ++ ", " ++ output_QF_BV set expr2 ++ ")"


instance Output BinOp where 
  output_QF_NIA Plus   = "+"
  output_QF_NIA Minus  = "-"
  output_QF_NIA Mult   = "*"
  output_QF_NIA Div    = "/"
  output_QF_NIA And    = "and"
  output_QF_NIA Or     = "or"
  output_QF_NIA Eq     = "="
  output_QF_NIA Neq    = error "Not Equal is not defined in SMT-LIB2 theory QF_NIA. Use (not (= . .) ) instead."
  output_QF_NIA ULeq   = "<="
  output_QF_NIA UGeq   = ">="
  output_QF_NIA ULth   = "<"
  output_QF_NIA UGth   = ">"
  output_QF_NIA Leq    = "<="
  output_QF_NIA Geq    = ">="
  output_QF_NIA Lth    = "<"
  output_QF_NIA Gth    = ">"
  output_QF_NIA BAnd   = "&"
  output_QF_NIA BOr    = "|"
  output_QF_NIA BXor   = "^"
  output_QF_NIA SLeft  = "<<"
  output_QF_NIA SRight = ">>"
  output_QF_NIA BitIdx = "BIT"

  output_QF_BV _ Plus   = "bvadd"
  output_QF_BV _ Minus  = "bvsub"
  output_QF_BV _ Mult   = "bvmul"
  output_QF_BV _ Div    = "bvdiv"
  output_QF_BV _ And    = "and"
  output_QF_BV _ Or     = "or"
  output_QF_BV _ Eq     = "="
  output_QF_BV _ Neq    = error "Not Equal is not defined in SMT-LIB2 theory QF_NIA. Use (not (= . .) ) instead."
  output_QF_BV _ ULeq   = "bvule"
  output_QF_BV _ UGeq   = "bvuge"
  output_QF_BV _ ULth   = "bvult"
  output_QF_BV _ UGth   = "bvugt"
  output_QF_BV _ Leq    = "bvsle"
  output_QF_BV _ Geq    = "bvuge"
  output_QF_BV _ Lth    = "bvslt"
  output_QF_BV _ Gth    = "bvsgt"
  output_QF_BV _ BAnd   = "bvand"
  output_QF_BV _ BOr    = "bvor"
  output_QF_BV _ BXor   = "bvxor"
  output_QF_BV _ SLeft  = "bvshl"
  output_QF_BV _ SRight = "bvshr"
  output_QF_BV _ BitIdx = "BIT"

instance Output UnOp where
  output_QF_NIA Not        = "not"
  output_QF_NIA BNot       = "not"
  output_QF_NIA Neg        = "-"
  output_QF_NIA ToInt      = "(int)"
  output_QF_NIA ToUnsigned = "(unsigned)"

  output_QF_BV _ Not = "not"
  output_QF_BV _ BNot = "not"
  output_QF_BV _ Neg = "neg"
  output_QF_BV _ ToInt = "(int)"
  output_QF_BV _ ToUnsigned = "(unsigned)"

instance Output Type where 
  output_QF_NIA (TVal t p)   = output_QF_NIA p ++ " Int"
  output_QF_NIA (TArray t _) = "Array Int" ++ output_QF_NIA t
  output_QF_NIA (TPointer t) = output_QF_NIA t ++ " *"
  output_QF_NIA TVoid        = "Void"
  output_QF_NIA TBool        = "Bool"

  output_QF_BV set (TVal t p)   = output_QF_BV set p ++ " " ++ output_QF_BV set t
  output_QF_BV set (TArray t Nothing)  = "Array" ++ "[] " ++ output_QF_BV set t
  output_QF_BV set (TArray t (Just l)) = "Array" ++ "[" ++ show l ++ "] " ++ output_QF_BV set t
  output_QF_BV set (TPointer t) = output_QF_BV set t ++ " *"
  output_QF_BV set TVoid        = "void"
  output_QF_BV set TBool        = "bool"

instance Output TypePolarity where
  output_QF_NIA Signed   = "signed"
  output_QF_NIA Unsigned = "unsigned"

  output_QF_BV _ Signed   = "signed"
  output_QF_BV _ Unsigned = "unsigned"

instance Output TypeVal where
  output_QF_NIA _ = ""

  output_QF_BV set TChar  = "(_ BitVec " ++ (show $ schar  $ varSize set) ++ ")"
  output_QF_BV set TShort = "(_ BitVec " ++ (show $ sshort $ varSize set) ++ ")"
  output_QF_BV set TInt   = "(_ BitVec " ++ (show $ sint   $ varSize set) ++ ")"
  output_QF_BV set TLong  = "(_ BitVec " ++ (show $ slong  $ varSize set) ++ ")"
