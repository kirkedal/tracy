-----------------------------------------------------------------------------
--
-- Module      :  AstPretty
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Pretty printing of Abstract syntax tree for subset of C++
-- |
-- |Not all parts are (needed to be) defined.
-----------------------------------------------------------------------------

module Tracy.AstPretty where

import Tracy.Ast
import Tracy.AstExec
import Data.List (intercalate)
import qualified BitVector as BV
import qualified Data.Array.IArray as A


-------------------------------------------------------------------------------
-- ** Pretty for showing programs and values
-------------------------------------------------------------------------------

class Pretty a where
  pretty :: a -> String

instance Pretty Func where
  pretty func = "\n" ++ pretty (retType func) ++ " " ++ funcname func ++ "(" ++ intercalate ", " (map pretty (args func)) ++ ")" ++
    concatMap pretty (body func)

instance Pretty Stmt where
  pretty (DefVar ident (TArray typ Nothing))  = "\n" ++ pretty typ ++ " " ++ ident ++ "[];"
  pretty (DefVar ident (TArray typ (Just i))) = "\n" ++ pretty typ ++ " " ++ ident ++ "[" ++ show i ++ "];" 
  pretty (DefVar ident (TPointer typ)) = "\n" ++ pretty typ ++ " &" ++ ident ++ ";" 
  pretty (DefVar ident typ)       = "\n" ++ pretty typ ++ " " ++ ident ++ ";"
  pretty (Assign ident expr) = ident ++ " = " ++ pretty expr ++ ";"
  pretty (Expression expr) = pretty expr ++ ";"
  pretty (AssignArray ident exprA expr) = "\n" ++ ident ++ "[" ++ pretty exprA ++ "] = " ++ pretty expr ++ ";"
  pretty (Return expr) = "\nreturn" ++ pretty expr ++ ";"

instance Pretty Expr where
  pretty (VarExpr ident) = ident
  pretty (StringExpr str) = str
  pretty (HexExpr int) = show int
  pretty (ArrayExpr ident expr) = ident ++ "[" ++ pretty expr ++ "]"
  pretty (TrueExpr) = "ctx.bool_val(true)"
  pretty (FalseExpr) = "ctx.bool_val(false)"
  pretty (ConstExpr _ c) = "ctx.bv_val(" ++ show c ++ ", bitwidth)"
  pretty (ErrorExpr _c) = ""
  pretty (UnOpExpr unOp expr) = pretty unOp ++ pretty expr
  pretty (BinOpExpr expr1 binOp expr2) = 
    if   elem binOp [ULeq, UGeq, ULth, UGth, BitIdx]
    then pretty binOp ++ "(" ++ pretty expr1 ++ "," ++ pretty expr2 ++ ")"
    else "(" ++ pretty expr1 ++ " " ++ pretty binOp ++ " " ++ pretty expr2 ++ ")"
  pretty (FunCallExpr ident exprs) = ident ++ "(" ++ intercalate ", " (map pretty exprs) ++ ")"
  pretty (CondExpr exprC expr1 expr2) = "ite(" ++ pretty exprC ++ ", " ++ pretty expr1 ++ ", " ++ pretty expr2 ++ ")"

instance Pretty BinOp where 
  pretty Plus  = " + "
  pretty Minus = " - "
  pretty Mult  = " * "
  pretty Div   = " / "
  pretty And   = "&&"
  pretty Or    = "||"
  pretty Eq    = "=="
  pretty Neq   = "!="
  pretty ULeq  = "ule"
  pretty UGeq  = "uge"
  pretty ULth  = "ult"
  pretty UGth  = "ugt"
  pretty Leq   = "<="
  pretty Geq   = ">="
  pretty Lth   = "<"
  pretty Gth   = ">"
  pretty BAnd  = "&"
  pretty BOr   = "|"
  pretty BXor  = "^"
  pretty SLeft = "<<"
  pretty SRight = ">>"
  pretty BitIdx = "BIT"

instance Pretty UnOp where
  pretty Not = "!"
  pretty BNot = "~"
  pretty Neg = "-"
  pretty ToInt = "(int)"
  pretty ToUnsigned = "(unsigned)"

instance Pretty Type where 
  pretty (TVal t p)   = pretty p ++ " " ++ pretty t
  pretty (TArray t Nothing)  = "Array" ++ "[] " ++ pretty t
  pretty (TArray t (Just l)) = "Array" ++ "[" ++ show l ++ "] " ++ pretty t
  pretty (TPointer t) = pretty t ++ " *"
  pretty TVoid        = "void"
  pretty TBool        = "bool"

instance Pretty Value where
  pretty (VVal _ Signed v)   = show $ BV.int v
  pretty (VVal _ Unsigned v) = (show $ BV.int v) ++ "u"
  pretty (VPointer _ i)  = "&" ++ i
  pretty (VArray _ _ _)  = "array..."
  pretty (VBool True)    = "True"
  pretty (VBool False)   = "False"
  pretty (VUnassigned _) = "Unassigned"
  pretty (VVoid)         = "Void"

instance Pretty TypeVal where
  pretty TChar  = "char"
  pretty TShort = "short"
  pretty TInt   = "int"
  pretty TLong  = "long"

instance  Pretty TypePolarity where
  pretty Signed   = "signed"
  pretty Unsigned = "unsigned"


-------------------------------------------------------------------------------
-- ** Pretty for showing C programs
-------------------------------------------------------------------------------

prettyCprog :: Program -> String
prettyCprog (globals, funs) = (concatMap prettyC globals) ++ (concatMap prettyC funs)

prettyCs :: [Stmt] -> String
prettyCs stmts = concatMap prettyC stmts


class PrettyC a where
  prettyC :: a -> String

instance PrettyC Func where
  prettyC func = "\n" ++ prettyC (retType func) ++ " " ++ 
      funcname func ++ "(" ++ intercalate ", " (map pC (args func)) ++ ") {" ++ 
      prettyCs (body func) ++ "\n}\n"
    where
      pC (DefVar ident (TArray typ Nothing))  = "\n" ++ pretty typ ++ " " ++ ident ++ "[]"
      pC (DefVar ident (TArray typ (Just i))) = "\n" ++ pretty typ ++ " " ++ ident ++ "[" ++ show i ++ "]" 
      pC (DefVar ident typ)       = prettyC typ ++ " " ++ ident
      -- pC _ = undefined

instance PrettyC Stmt where
  prettyC (DefVar ident (TArray typ Nothing))  = "\n" ++ pretty typ ++ " " ++ ident ++ "[];"
  prettyC (DefVar ident (TArray typ (Just i))) = "\n" ++ pretty typ ++ " " ++ ident ++ "[" ++ show i ++ "];" 
  prettyC (DefVar ident typ)       = "\n" ++ prettyC typ ++ " " ++ ident ++ ";"
  prettyC (Assign ident expr) = "\n" ++ ident ++ " = " ++ prettyC expr ++ ";"
  prettyC (Expression expr) = "\n" ++ prettyC expr ++ ";"
  prettyC (AssignArray ident exprA expr) = "\n" ++ ident ++ "[" ++ prettyC exprA ++ "] = " ++ prettyC expr ++ ";"
  prettyC (Return expr) = "\nreturn " ++ prettyC expr ++ ";"
  prettyC (Cond expr stmt1 stmt2) = "\nif (" ++ prettyC expr ++ ") {" ++ prettyCs stmt1 ++ "\n} else {" ++ prettyCs stmt2 ++ "\n}"
  prettyC (While expr stmt) = "\nwhile (" ++ prettyC expr ++ ") {" ++ prettyCs stmt ++ "\n}"
  prettyC (Assert int expr) = "// SPEC# " ++ show int ++ " : assert(" ++ prettyC expr ++ ");"
  prettyC (BlockStmt bname stmts) = "\n// BEGIN(" ++ bname ++ ")" ++ prettyCs stmts ++ "\n// END(" ++ bname ++ ")"
  prettyC (Spec string) = "\n// SPEC_BEGIN " ++ string ++ "//SPEC_END"

instance PrettyC Expr where
  prettyC (VarExpr ident) = ident
  prettyC (StringExpr str) = "\"" ++ str ++ "\""
  prettyC (HexExpr int) = show int
  prettyC (ArrayExpr ident expr) = ident ++ "[" ++ prettyC expr ++ "]"
  prettyC (TrueExpr) = "true"
  prettyC (FalseExpr) = "false"
  prettyC (IncDecExpr IncPre ident)  = "++" ++ ident
  prettyC (IncDecExpr IncPost ident) = ident ++ "++"
  prettyC (IncDecExpr DecPre ident)  = "--" ++ ident
  prettyC (IncDecExpr DecPost ident) = ident ++ "--"
  prettyC (PReference ident) = "&" ++ ident
  prettyC (ConstExpr Signed c) = show c
  prettyC (ConstExpr Unsigned c) = show c ++ "u"
  prettyC (ErrorExpr expr) = "/*ERROR_BEGIN*/ " ++ prettyC expr ++ "/*ERROR_END*/"
  prettyC (UnOpExpr unOp expr) = prettyC unOp ++ prettyC expr
  prettyC (BinOpExpr expr1 binOp expr2) = 
    "(" ++ prettyC expr1 ++ " " ++ prettyC binOp ++ " " ++ prettyC expr2 ++ ")"
  prettyC (FunCallExpr ident exprs) = ident ++ "(" ++ intercalate ", " (map prettyC exprs) ++ ")"
  prettyC (CondExpr exprC expr1 expr2) = "(" ++ prettyC exprC ++ " ? " ++ prettyC expr1 ++ " : " ++ prettyC expr2 ++ ")"


instance PrettyC BinOp where 
  prettyC Plus  = " + "
  prettyC Minus = " - "
  prettyC Mult  = " * "
  prettyC Div   = " / "
  prettyC And   = " && "
  prettyC Or    = " || "
  prettyC Eq    = " == "
  prettyC Neq   = " != "
  prettyC ULeq  = " <= "
  prettyC UGeq  = " >= "
  prettyC ULth  = " < "
  prettyC UGth  = " > "
  prettyC Leq   = " <= "
  prettyC Geq   = " >= "
  prettyC Lth   = " < "
  prettyC Gth   = " > "
  prettyC BAnd  = "&"
  prettyC BOr   = "|"
  prettyC BXor  = "^"
  prettyC SLeft = "<<"
  prettyC SRight = ">>"
  prettyC BitIdx = "BIT"

instance PrettyC UnOp where
  prettyC Not = "!"
  prettyC BNot = "~"
  prettyC Neg = "-"

instance PrettyC Type where 
  prettyC (TVal t p)   = prettyC p ++ " " ++ prettyC t
  prettyC TBool        = "bool"
  prettyC TVoid        = "void"
  prettyC (TArray t l) = "array"  -- More print

instance PrettyC TypeVal where
  prettyC TChar  = "char"
  prettyC TShort = "short"
  prettyC TInt   = "int"
  prettyC TLong  = "long"

instance  PrettyC TypePolarity where
  prettyC Signed   = "signed"
  prettyC Unsigned = "unsigned"


-------------------------------------------------------------------------------
-- ** Helper functions
-------------------------------------------------------------------------------

isBoolExpr :: Expr -> Bool
isBoolExpr  TrueExpr             = True
isBoolExpr  FalseExpr            = True
isBoolExpr (UnOpExpr Not _)      = True
isBoolExpr (BinOpExpr _ binOp _) = isBoolBinOp binOp
isBoolExpr _                     = False

