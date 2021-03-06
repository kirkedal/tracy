-----------------------------------------------------------------------------
--
-- Module      :  Ast
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  none?
-- Portability :
--
-- |Abstract syntax tree for subset of C++
--
-----------------------------------------------------------------------------

module Tracy.Ast where

import qualified BitVector as BV
import qualified Data.Array.IArray as A
import qualified Text.Parsec.Pos as P

type SourcePos = P.SourcePos
initSourcePos = P.initialPos ""

-- |A program is a sequence of functions and global definitions
type Program  = ([Stmt], [Func])

-- |Right now a constant is a simple integer
type Const    = Integer

-- |Identifiers are simple strings
type Ident    = String

-- |A function
data Func     = Func { retType   :: Type      -- ^ Return type
                     , funcname  :: Ident     -- ^ Name of the function
                     , args      :: [Stmt]    -- ^ The inputs
                     , body      :: [Stmt]}   -- ^ Function body
              deriving (Eq, Show)

-- |A statement
data Stmt     = DefVar Ident Type SourcePos
              | Expression Expr SourcePos
              | AssignVar Ident Expr SourcePos
              | AssignArray Ident Expr Expr SourcePos
              | Return Expr SourcePos
              | Cond Expr [Stmt] [Stmt] SourcePos
              | While Expr [Stmt] SourcePos
              | AssertStmt Int Expr SourcePos
              | BlockStmt Ident [Stmt] SourcePos SourcePos 
              | Spec String SourcePos
              | Injection Action SourcePos
              deriving (Show, Eq)

-- |Expression 
data Expr     = VarExpr Ident               
              | StringExpr String           
              | HexExpr Integer             
              | ArrayExpr Ident Expr        
              | TrueExpr                 
              | FalseExpr                
              | IncDecExpr IncDec Ident
              | ConstExpr TypePolarity Const   
              | UnOpExpr  UnOp Expr            
              | BinOpExpr Expr BinOp Expr      
              | FunCallExpr Ident [Expr]       
              | CondExpr Expr Expr Expr        
              | ErrorExpr Expr
              | PReference Ident
              deriving (Show, Eq)

-- |Increment and decrement operators
data IncDec   = IncPre
              | IncPost
              | DecPre
              | DecPost
              deriving (Show, Eq)


-- |Binary operators
data BinOp    = Plus            -- ^ 
              | Minus           -- ^ 
              | Mult            -- ^ 
              | Div             -- ^ 
              | And             -- ^ 
              | Or              -- ^ 
              | Eq              -- ^ 
              | Neq             -- ^ 
              | ULeq            -- ^ 
              | UGeq            -- ^ 
              | ULth            -- ^ 
              | UGth            -- ^ 
              | Leq             -- ^ 
              | Geq             -- ^ 
              | Lth             -- ^ 
              | Gth             -- ^ 
              | BAnd
              | BOr
              | BXor
              | SLeft
              | SRight
              | BitIdx
              deriving (Show, Eq)



-- |All unary operations
data UnOp     = Not                            -- ^ 
              | Neg                            -- ^ 
              | BNot
              | ToInt
              | ToUnsigned
              deriving (Show, Eq)

-- |Polarity of a value
data TypePolarity = Signed | Unsigned
          deriving (Eq, Show)

-- |Basic integer types
data TypeVal = TChar         -- ^ Chars is size of an Integer
             | TShort 
             | TInt          -- ^ Int is size of an Integer
             | TLong  
          deriving (Eq, Show)

-- |Types 
data Type = TVal TypeVal TypePolarity
          | TVoid
          | TArray Type (Maybe Integer)
          | TBool
          | TPointer Type
          | TString
          deriving (Eq, Show)

-- |Values
data Value = VVal        TypeVal TypePolarity BV.BitVector
           | VArray      TypeVal TypePolarity Array
           | VUnassigned Type
           | VString     String
           | VBool       Bool
           | VVoid
           | VPointer    Type Ident
           deriving (Show)

type Array = A.Array Integer BV.BitVector


instance Eq Value where
  (==) (VVal _t1 _p1 v1) (VVal _t2 _p2 v2) = (==) v1 v2
  (==) _ _ = error "Wrong types in =="


-------------------------------------------------------------------------------
-- ** Actions
-------------------------------------------------------------------------------

-- Error numbers are not handled yet
data Action     = Decl    SourcePos ErrorNumber Type Ident
                | Assume  SourcePos ErrorNumber Expr
                | Assign  SourcePos ErrorNumber Ident Expr
                | AssignA SourcePos ErrorNumber Ident Expr Expr
                | Assert  SourcePos ErrorNumber Expr
                | Error   SourcePos Action
                | Debug   SourcePos String
                | Begin   SourcePos String
                | End     SourcePos String
                deriving (Eq, Show)

type ErrorNumber = String


