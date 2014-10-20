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
        pa []                           set = return set
        pa ("--filename":str:l)         set = pa l set{ filename = str }
        pa ("--function":str:l)         set = pa l set{ function = str }
        pa ("--assignment":"random":l)  set = pa l set{ assignment = InputRandom }
        pa ("--assignment":"zero":l)    set = pa l set{ assignment = InputZero }
        pa ("--assignment":str:l)       set = pa l set{ assignment = InputValues str [] }
        pa ("--int-width":str:l)        set = pa l set{ int_width = read str :: Int}
        pa ("--config":str:l)           set = pa l set{ config = str }
        pa ("--precond":str:l)          set = pa l set{ precondition = str }
        pa ("--postcond":str:l)         set = pa l set{ postcondition = str }
        pa ("--repairVar":str:l)        set = pa l set{ repairVar = Just str }
        pa ("--strip-out":str:l)        set = pa l set{ strip_out = splitWhenever ((==) ' ') str }
        pa ("--output":"Z3Int":l)       set = pa l set{ outputFormat = Z3Int }
        pa ("--output":"Z3Bit":l)       set = pa l set{ outputFormat = Z3Bit }
        pa ("--output":"Ccode":l)       set = pa l set{ outputFormat = Ccode }
        pa ("--output":out:l)           set = error $ "Output format unknown: " ++ out ++ "\nChoose from [Z3Int, Z3Bit, Ccode]"
        pa ("--pp-output":str:l)        set = pa l set{ ppOutFile = Just str }
        pa ("--3AC":l)                  set = pa l set{ proc3AC = True }
        pa ("--SSA":l)                  set = pa l set{ procSSA = True }
        pa ("--debug":l)                set = pa l set{ debugMode = True }
        pa ("--help":l)                 set = error usage
        pa (flag:l)                     _   = error $ "Invalid flag: " ++ flag
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
     ++ "    --int-width [int]   With of a standard integer\n"
     ++ "    --precond [expr]    Precondition as an expression\n"
     ++ "    --postcond [expr]   Postcondition as an expression\n"
     ++ "    --repairVar [str]   Name of variable to repair\n"
     ++ "    --strip-out [strs]  Space separated list of blocks that is to be ignored\n"
     ++ "    --output [list]     Output format: Choose from [Z3Int, Z3Bit, Ccode]\n"
     ++ "    --pp-output [file]  Write preparsed C code to file\n"
     ++ "    --3AC               Perform 3 address code transformation\n"
     ++ "    --SSA               Perform static single assignment transformation on trace\n"
     ++ "    --debug             More verbose output trace\n"
     ++ "    --help              This menu"

-------------------------------------------------------------------------------
-- ** Parsing program and running the tracer
-------------------------------------------------------------------------------

parseAndRun :: Settings -> IO ()
parseAndRun settings =
    do  (def, funcs) <- fromEither =<< parseInput (filename settings)
        updSettings  <- parseAssignment settings
        preExpr      <- fromEither =<< parseExpr (precondition settings) "Precondition" 
        postExpr     <- fromEither =<< parseExpr (postcondition settings) "Postcondition" 
        let astStripped  = (def, map (\f -> foldl filterBlock f (strip_out settings)) funcs)
            astProcessed = preprocess settings astStripped
        outputToPPFile astProcessed settings
        evalTrace    <- interp astProcessed updSettings preExpr postExpr
        putStrLn $ TA.produceOutput settings evalTrace
        repairMaybe $ repairVar settings
    where
        repairMaybe Nothing   = return ()
        repairMaybe (Just s)  = putStrLn $ "repair_variable := " ++ s
        filterBlock func s = func {body = filter (filterFunc s) $ body func}
        filterFunc s (BlockStmt p _)
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
      do  v <- fromEither =<< parseValue s (int_width set)
          return $ set {assignment = InputValues s v }
    otherwise -> return set

parseInput :: String -> IO (Either ParseError Program)
parseInput "-"      = parseString =<< getContents
parseInput filename = parseFromFile filename

fromEither :: Show a => Either a b -> IO b
fromEither (Right a)  = return a
fromEither (Left err) = putStr (show err ++ "\n") >> (exitWith $ ExitFailure 1)
