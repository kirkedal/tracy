-----------------------------------------------------------------------------
--
-- Module      :  SimpCParser
-- Copyright   :  Michael Kirkedal Thomsen, 2014
-- License     :  AllRightsReserved
--
-- Maintainer  :  Michael Kirkedal Thomsen kirkedal@informatik.uni-bremen.de
-- Stability   :  Baser
-- Portability :
--
-- |A simple parser for C code
--
-----------------------------------------------------------------------------

module Tracy.SimpCParser (parseString, parseFromFile, parseValue, parseExpr, ParseError) where

import Tracy.Ast
import Tracy.AstExec (arrayFromList)
import BitVector (bitVec)

import Text.ParserCombinators.Parsec hiding (parse,parseFromFile)
import qualified Text.Parsec.Token as P
import qualified Text.Parsec.Char as PC
import Text.Parsec.Prim (runP)
import qualified Text.ParserCombinators.Parsec.Expr as E

import Control.Monad
import Data.Maybe (fromMaybe, maybeToList)

-------------------------------------------------------------------------------
-- ** Main functions
-------------------------------------------------------------------------------

parse :: LangParser a -> String -> String -> Either ParseError a
parse p = runP p initialState

parseFromFile :: String -> IO (Either ParseError Program)
parseFromFile fname
    = do input <- readFile fname
         return (parse program fname input)

parseString :: String -> IO (Either ParseError Program)
parseString input = return $ parse program "stdIn" input

parseValue :: String -> Int -> IO (Either ParseError [Value])
parseValue input size = return $ parse (value size) "Assignments" input

parseExpr :: String -> String -> IO (Either ParseError Expr)
parseExpr input place = return $ parse expression place input

-------------------------------------------------------------------------------
-- ** Language definition
-------------------------------------------------------------------------------

type ParserState = [String]

initialState :: ParserState
initialState = []
type LangParser = GenParser Char ParserState

-- |Definition of the language
cStyle = P.LanguageDef {
  P.commentStart    = "",
  P.commentEnd      = "",
  P.commentLine     = "",
  P.nestedComments  = False, 
  P.identStart      = letter <|> char '_', 
  P.identLetter     = alphaNum <|> char '_',
  P.opStart         = oneOf ":!#$%&*+./<=>?@\\^|-~", 
  P.opLetter        = oneOf ":!#$%&*+./<=>?@\\^|-~", 
  P.reservedOpNames = ["=", "==", "+", "-", ">", "<", "*", "/", "++", "--", "&&", "||", ">=", "<=", "!", "#", "/*", "*/", "&", "|" ,"^"],
  P.reservedNames   = ["if", "else", "return", "int", "short", "long", "do", "while", "unsigned", "signed", "char", "void", "SPEC", "assert", "true", "false","ERROR_BEGIN", "ERROR_END", "ERROR", "BEGIN", "END"],
  P.caseSensitive   = True
}


-------------------------------------------------------------------------------
-- ** General parsers
-------------------------------------------------------------------------------

-- |Used to conveniently create the parsers 'natural', 'constant', and 'identifier'
--lexer :: Monad m0 => P.GenTokenParser [Char] st m0
lexer = P.makeTokenParser cStyle

-- |Parses a natural number
natural :: CharParser st Integer
natural = P.natural lexer

-- |Lex
lexeme :: CharParser st a -> CharParser st a
lexeme = P.lexeme lexer

-- |Parses white space
whiteSpace :: CharParser st ()
whiteSpace = P.whiteSpace lexer

-- |Parses the string s and skips trailing whitespaces
symbol :: String -> CharParser st String
symbol = P.symbol lexer

-- |Parses and returns a valid FDL identifier
identifier :: CharParser st String
identifier = P.identifier lexer

-- |Parses a constant (i.e. a number)
constant :: CharParser st Const
constant = lexeme natural >>= return . fromIntegral
--constant = lexeme natural >>= return . fromIntegral

-- |Parses a hexadecimal number
hexadecimal :: CharParser st Const
hexadecimal = P.hexadecimal lexer >>= return . fromIntegral

-- |Parser @(parens p)@ parses p and trailing whitespaces enclosed in parenthesis ('(' and ')'),
--  returning the value of p.
parens :: CharParser st a -> CharParser st a
parens = P.parens lexer

-- |Parser @(brackets p)@ parses p and trailing whitespaces enclosed in square brackets ('[' and ']'),
--  returning the value of p.
brackets :: CharParser st a -> CharParser st a
brackets = P.squares lexer

-- |Parser fro braces
braces :: CharParser st a -> CharParser st a
braces = P.braces lexer

-- |Parser for reserved
reserved :: String -> CharParser st ()
reserved = P.reserved lexer

stringLiteral :: CharParser st String
stringLiteral = P.stringLiteral lexer

-------------------------------------------------------------------------------
-- ** The C Language 
-------------------------------------------------------------------------------

-- |A program is global definitions followed be functions
program :: LangParser Program
program = do g <- globals
             f <- functions
             return (g,f)


globals :: LangParser [Stmt]
globals = do whiteSpace 
             gs <- many def
             return $ concat gs

def :: LangParser [Stmt]
def = try array <|> try var
  where
    var = do  t <- cType
              is <- sepBy1 identifier (symbol ",")
              symbol ";"
              return $ map (\x -> DefVar x t) is
    array = do  t <- cType
                i <- identifier
                s <- brackets constant
                symbol ";"
                return $ [DefVar i (TArray t (Just s))]

-- |Functions are at least 1 function
functions :: LangParser [Func]
functions = many1 function

-- |A function
function :: LangParser Func
function = do t <- cType
              i <- identifier
              a <- parens (definition `sepBy` (symbol ","))
              s <- braces statements
              return Func { retType = t, funcname = i, args = a, body = s }

-- |A type
cType :: LangParser Type
cType = try (reserved "void"  >> (return $ TVoid))
    <|> try (reserved "bool"  >> (return $ TBool))
    <|> try signed
    <|> try unsigned
    <|> try (reserved "char"  >> (return $ TVal TChar Unsigned))
    <|> typeOnly
    where
        signed   = do reserved "signed"
                      t <- cTypeSize
                      return $ TVal t Signed
        unsigned = do reserved "unsigned"
                      tM <- optionMaybe cTypeSize
                      let t = fromMaybe TInt tM
                      return $ TVal t Unsigned
        typeOnly = do t <- cTypeSize
                      return $ TVal t Signed

cTypeSize :: LangParser TypeVal
cTypeSize = try (reserved "char"  >> (return TChar))
        <|> try (reserved "int"   >> (return TInt))
        <|> try (reserved "long"  >> (return TLong))
        <|> (reserved "short" >> (return TShort))

-- |Definitions
definition :: LangParser Stmt
definition = do t <- cType
                p <- optionMaybe $ symbol "*"
                i <- identifier 
                a <- optionMaybe $ brackets $ optionMaybe constant
                return $ case (p,a) of
                    (Nothing, Nothing)   -> DefVar i t
                    (Nothing, Just(len)) -> DefVar i (TArray t len)
                    (Just(_), Nothing)   -> DefVar i (TPointer t)
                    (Just(_), Just(len)) -> DefVar i (TPointer (TArray t len))

-- |Statements are many
statements :: LangParser [Stmt]
statements = liftM concat (many statement)

-- |Single statement
statement :: LangParser [Stmt]
statement = try basic <|> try cond <|> try condS <|> try ret <|> try assert <|> try errorS <|> try block <|> try specS
        <|> try basic <|> try def <|> try assignA <|> try for <|> try while <|> doWhile
  where
    basic   = do b <- basicStatement
                 symbol ";"
                 return b
    assignA = do a <- identifier
                 s <- brackets expression
                 symbol "="
                 e <- expression
                 symbol ";"
                 return $ [AssignArray a s e]
    condS = do   reserved "if"
                 e  <- parens expression
                 s1 <- choice [braces statements, statement]
                 return $ [Cond e s1 []]
    cond = do    reserved "if"
                 e  <- parens expression
                 s1 <- choice [braces statements, statement]
                 reserved "else"
                 s2 <- choice [braces statements, statement]
                 return $ [Cond e s1 s2]
    while = do   reserved "while"
                 e <- parens expression
                 s <- choice [braces statements, statement]
                 return $ [While e s]
    doWhile = do reserved "do"
                 s <- choice [braces statements, statement]
                 reserved "while"
                 e <- parens expression
                 symbol ";"
                 return $ s ++ [While e s]
    for     = do reserved "for"
                 symbol "("
                 as <- optionMaybe for_ass
                 symbol ";"
                 c <- expression
                 symbol ";"
                 i <- basicStatement
                 symbol ")"
                 s <- choice [braces statements, statement]
                 return $ case as of
                    Nothing    -> [While c (s ++ i)]
                    Just (a,e) -> (AssignVar a e):[While c (s ++ i)]
    for_ass = do a <- identifier
                 symbol "="
                 e <- expression
                 return (a,e)
    ret = do     reserved "return"
                 e <- expression
                 symbol ";"
                 return $ [Return e]
    assert  = do reserved "assert"
                 p <- getPosition
                 e <- parens expression
                 symbol ";"
                 return $ [AssertStmt (sourceLine p) e]
    errorS = do  symbol "/*"
                 reserved "ERROR_BEGIN"
                 symbol "*/"
                 s <- statements
                 symbol "/*"
                 reserved "ERROR_END"
                 symbol "*/"
                 return $ [BlockStmt "Error" s]
    block  = do  symbol "//"
                 reserved "BEGIN"
                 bname <- parens identifier
                 s <-  statements 
                 symbol "//"
                 reserved "END"
                 parens $ string bname
                 return $ [BlockStmt bname s]
    specS  = do  symbol "//"
                 reserved "SPEC_BEGIN"
                 c <- manyTill ( (PC.anyChar)) specE
                 return $ [Spec c]
    specE  = do  symbol "//"
                 reserved "SPEC_END"

-- |Some basic statements
basicStatement :: LangParser [Stmt]
basicStatement = try passign <|> try assign <|> try expr <|> defAss
  where
    passign = do i <- identifier
                 o <- pOpps
                 e <- expression
                 return $ [AssignVar i (BinOpExpr (VarExpr i) o e)]
    expr   = do  e <- expression
                 return $ [Expression e]
    assign = do  a <- identifier
                 symbol "="
                 e <- expression
                 return $ [AssignVar a e]
    defAss = do  t <- cType
                 p <- optionMaybe $ symbol "*"
                 i <- identifier
                 l <- optionMaybe $ brackets constant
                 e <- optionMaybe $ symbol "=" >> expression
                 let a = map (AssignVar i) $ maybeToList e 
                 return $ case (p,l) of
                    (Nothing, Nothing) -> (DefVar i t):a
                    (Just _,  Nothing) -> (DefVar i (TPointer t)):a
                    (Nothing, size)    -> (DefVar i (TArray t size)):a
                    (Just _,  size)    -> (DefVar i (TPointer (TArray t size))):a


-- |Parses a expression using the buildExpressionParser
expression :: LangParser Expr
expression = E.buildExpressionParser opTable basicExpressions
  where
    basicExpressions  = try condExpression2
                    <|> try funCallExpression
                    <|> try arrayExpression
                    <|> try hexExpression
                    <|> try bitExpression
                    <|> try varExpression
                    <|> try pointerReference
                    <|> try stringExpression
                    <|> try constantUnSignExpr
                    <|> try constantExpression
                    <|> try condExpression1
                    <|> try errorExpression
                    <|> try (reserved "true" >> return TrueExpr)
                    <|> try (reserved "false" >> return FalseExpr)
                    <|> parens expression
    arrayExpression    = ArrayExpr `liftM2` identifier $ brackets expression 
    varExpression      = liftM VarExpr identifier 
    pointerReference   = symbol "&" >> liftM PReference identifier 
    stringExpression   = liftM StringExpr stringLiteral
    constantUnSignExpr = do c <- constant
                            symbol "u"
                            return $ ConstExpr Unsigned c
    constantExpression = ConstExpr Signed `liftM` constant
    errorExpression    = do symbol "/*"
                            reserved "ERROR_BEGIN"
                            symbol "*/"
                            e <- expression
--                            symbol "/*"
                            reserved "ERROR_END"
                            symbol "*/"
                            return $ ErrorExpr e
    bitExpression      = do reserved "BIT"
                            symbol "("
                            e1 <- expression
                            symbol ","
                            e2 <- expression
                            symbol ")"
                            return $ BinOpExpr e1 BitIdx e2
    hexExpression      = do lookAhead (symbol "0x")
                            c <- hexadecimal
                            return $ HexExpr c
    condExpression1    = do symbol "("
                            e1 <- expression
                            symbol "?"
                            e2 <- expression
                            symbol ":"
                            e3 <- expression
                            symbol ")"
                            return $ CondExpr e1 e2 e3 
    condExpression2    = do symbol "ite"
                            symbol "("
                            e1 <- expression
                            symbol ","
                            e2 <- expression
                            symbol ","
                            e3 <- expression
                            symbol ")"
                            return $ CondExpr e1 e2 e3 
    funCallExpression  = FunCallExpr `liftM2` identifier $ parens (sepBy expression (symbol ","))

-- |Operator table for expressions
opTable :: [[ E.Operator Char st Expr ]]
opTable = [
  -- Operators listed from highest precedence to lowest precedence.
    [ errorExpr ],
    [ unopPre "--" DecPre, unopPre "++" IncPre],
    [ unopPost "--" DecPost, unopPost "++" IncPost],
    [ unop "-" Neg, unop "!" Not, unop "~" BNot],
    [ op "*"  Mult  E.AssocLeft, op "/"  Div    E.AssocLeft ],
    [ op "+"  Plus  E.AssocLeft, op "-"  Minus  E.AssocLeft ],
    [ op "<<" SLeft E.AssocLeft, op ">>" SRight E.AssocLeft ],
    [ op "<=" Leq   E.AssocLeft, op ">=" Geq    E.AssocLeft,
      op "<"  Lth   E.AssocLeft, op ">"  Gth    E.AssocLeft ],
    [ op "==" Eq    E.AssocLeft, op "!=" Neq    E.AssocLeft ],
    [ op "&"  BAnd  E.AssocLeft ],
    [ op "|"  BOr   E.AssocLeft ],
    [ op "^"  BXor  E.AssocLeft ],
    [ op "&&" And   E.AssocLeft ],
    [ op "||" Or    E.AssocLeft ]
  ]
  where
    op s f assoc = E.Infix   (try(do { symbol s ; notFollowedBy $ symbol s ; return $ \a b -> BinOpExpr a f b })) assoc
    unop s f     = E.Prefix  (try(do { symbol s ; return $ \a -> UnOpExpr f a }))
    unopPre s f  = E.Prefix  (try(do { symbol s ; lookAhead identifier ; return $ \(VarExpr a) -> IncDecExpr f a }))
    unopPost s f = E.Postfix (try(do { symbol s ; return $ \(VarExpr a) -> IncDecExpr f a }))
    errorExpr    = E.Postfix (try(do { symbol "/*" ; return $ \a -> a }))

-- |Assigment operators
pOpps :: LangParser BinOp
pOpps = (symbol "|=" >> return BOr)
    <|> (symbol "&=" >> return BAnd)
    <|> (symbol "+=" >> return Plus)
    <|> (symbol "-=" >> return Minus)
    <|> (symbol "/=" >> return Div)
    <|> (symbol "*=" >> return Mult)
    <|> (symbol ">>=" >> return SRight)
    <|> (symbol "<<=" >> return SLeft)

-------------------------------------------------------------------------------
-- ** Parsing input values
-------------------------------------------------------------------------------

value :: Int -> LangParser [Value]
value size = try vals <|> oneValL
  where
    oneValL = do c <- oneVal
                 return [c]
    oneVal = arr <|> val
    vals = do parens (oneVal `sepBy1` (symbol ","))
    val = do  v <- cons 
              return $ VVal TInt Signed $ bitVec size v
    cons = try constant <|> ncon
    arr  = do c <- braces $ cons `sepBy1` (symbol ",")
              return $ VArray TInt Signed $ arrayFromList $ map (bitVec size) c
    ncon = 
      do  symbol "-" 
          c <- constant
          return $ -c
