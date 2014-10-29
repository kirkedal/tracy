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
-- |The main executable for the C parser to generate execTrace
--
-- This is supposed to become the compiler
--
-----------------------------------------------------------------------------

module Main (main) where

import Tracy.Ast
import Tracy.Types
import Tracy.AstPretty (prettyCprog)
import qualified Tracy.TraceAst as TA
import Tracy.SimpCParser (parseFromFile, parseString, parseValue, parseExpr, ParseError)
import Tracy.PreProcess (preprocess, normBoolCond)
import Tracy.Trace 
import Tracy.LineInjecter
import Tracy.NormError (normError)

import System.Environment
import System.Exit

main :: IO ()
main = do arguments <- getArgs
          settings  <- parseArgs arguments
          parseAndRun settings

-------------------------------------------------------------------------------
-- ** Parsing arguments
-------------------------------------------------------------------------------

parseArgs :: [String] -> IO Settings
parseArgs args = pa args emptySettings
    where
        pa []                                 set = return set
        pa ("--filename":str:l)               set = pa l set{ filename = str }
        pa ("--function":str:l)               set = pa l set{ function = str }
        pa ("--assignment":"random":l)        set = pa l set{ assignment = InputRandom }
        pa ("--assignment":"zero":l)          set = pa l set{ assignment = InputZero }
        pa ("--assignment":str:l)             set = pa l set{ assignment = InputValues str [] }
        pa ("--bit-width-abstraction":str:l)  set = pa l set{ varSize = (varSize set){ sint = read str :: Int } }
        pa ("--config":str:l)                 set = pa l set{ config = str }
        pa ("--precond":str:l)                set = pa l set{ precondition = str }
        pa ("--postcond":str:l)               set = pa l set{ postcondition = str }
        pa ("--repairVar":str:l)              set = pa l set{ repairVar = Just str }
        pa ("--strip-out":str:l)              set = pa l set{ strip_out = splitWhenever ((==) ' ') str }
        pa ("--format":"QF_NIA":l)            set = pa l set{ outputFormat = SMT2_QF_NIA }
        pa ("--format":"QF_BV":l)             set = pa l set{ outputFormat = SMT2_QF_BV }
        pa ("--format":"Ccode":l)             set = pa l set{ outputFormat = Ccode }
        pa ("--format":out:l)                 set = error $ "Output format unknown: " ++ out ++ "\nChoose from [QF_NIA, QF_BV, Ccode]"
        pa ("--pp-output":str:l)              set = pa l set{ ppOutFile = Just str }
        pa ("--3AC":l)                        set = pa l set{ proc3AC = True }
        pa ("--SSA":l)                        set = pa l set{ procSSA = True }
        pa ("--inject":str:l)                 set = pa l set{ injections = str:(injections set) }
        pa ("--block-ann":blk:str:l)          set = pa l set{ blockAnnotation = (blk,str):(blockAnnotation set) }
        pa ("--debug":l)                      set = pa l set{ debugMode = True }
        pa ("--linenumbers":l)                set = pa l set{ linenumbers = True }
        pa ("--help":l)                       set = error usage
        pa (flag:l)                           _   = error $ "Invalid flag: " ++ flag
        splitWhenever f l = splitWhenever_ f l []
        splitWhenever_ _ []     []     = []
        splitWhenever_ _ []     (r:rs) = (reverse r):rs
        splitWhenever_ f (l:ls) []     
          | f l       = splitWhenever_ f ls []
          | otherwise = splitWhenever_ f ls [[l]]
        splitWhenever_ f (l:ls) (r:rs)
          | f l       = splitWhenever_ f ls $ []:(reverse r):rs
          | otherwise = splitWhenever_ f ls $ (l:r):rs

usage :: String
usage = "Usage of tracer: Tracer \n"
     ++ "    --filename [file]   Filename of the C file; \"main.c\" as standard\n"
     ++ "    --function [str]    Function name of the top function; \"main\" as standard\n"
     ++ "    --assignment a      List of comma separated assignments \"(a,b,c)\", \"random\" or \"zero\"\n"
     ++ "    --bit-width-abstraction [int]   Reduce integer bit-width\n"
     ++ "    --precond [expr]    Precondition as an expression\n"
     ++ "    --postcond [expr]   Postcondition as an expression\n"
     ++ "    --repairVar [str]   Name of variable to repair\n"
     ++ "    --strip-out [strs]  Space separated list of blocks that is to be ignored\n"
     ++ "    --format [list]     Output format: Choose from [QF_NIA, QF_BV, Ccode]\n"
     ++ "    --pp-output [file]  Write preparsed C code to file\n"
     ++ "    --3AC               Perform 3 address code transformation\n"
     ++ "    --SSA               Perform static single assignment transformation on trace\n"
     ++ "    --inject [lin:code] Updates the code in line \"lin\" to the code in \"code\"\n"
     ++ "    --block-ann [b] [i] Inserts a block with make b around lines in interval i of form \"l1-l2\"\n"
     ++ "    --linenumbers       Show linenumbers before each action\n"
     ++ "    --debug             More verbose output trace\n"
     ++ "    --help              This menu"

-------------------------------------------------------------------------------
-- ** Parsing program and running the tracer
-------------------------------------------------------------------------------

parseAndRun :: Settings -> IO ()
parseAndRun settings =
    do  injectCode   <- parseInjection settings
        (def, funcs) <- parseInput injectCode (filename settings)
        updSettings  <- parseAssignment settings
        preExpr      <- fromEither =<< parseExpr (precondition settings) "Precondition" 
        postExpr     <- fromEither =<< parseExpr (postcondition settings) "Postcondition" 
        let astStripped  = (def, map (\f -> foldl filterBlock f (strip_out settings)) funcs)
            astErrorNorm = normError astStripped
            astProcessed = preprocess settings astErrorNorm
        outputToPPFile astProcessed settings
        -- putStrLn $ show astProcessed
        evalTrace    <- interp astProcessed updSettings preExpr postExpr
        putStrLn $ TA.produceOutput settings evalTrace
        repairMaybe $ repairVar settings
    where
        repairMaybe Nothing   = return ()
        repairMaybe (Just s)  = putStrLn $ "repair_variable := " ++ s
        filterBlock func s = func {body = filter (filterFunc s) $ body func}
        filterFunc s (BlockStmt p _ _ _)
          | s == p    = False
          | otherwise = True
        filterFunc _ _ = True

outputToPPFile :: Program -> Settings -> IO()
outputToPPFile prg set =
    case ppOutFile set of 
        Nothing       -> return ()
        Just(outfile) -> writeFile outfile $ prettyCprog prg

parseAssignment :: Settings -> IO Settings
parseAssignment set =
  case assignment set of
    (InputValues s _) ->
      do  v <- fromEither =<< parseValue s (sint $ varSize set)
          return $ set {assignment = InputValues s v }
    otherwise -> return set

parseInput :: String -> String -> IO Program
parseInput "-"        _        = fromEither =<< parseString "stdIn" =<< getContents
parseInput codeString filename = fromEither =<< parseString filename codeString

parseInjection :: Settings -> IO String
parseInjection settings = injection settings

fromEither :: Show a => Either a b -> IO b
fromEither (Right a)  = return a
fromEither (Left err) = putStr (show err ++ "\n") >> (exitWith $ ExitFailure 1)
