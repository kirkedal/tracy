-----------------------------------------------------------------------------
--
-- Module      :  LineInjecter
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

module Tracy.LineInjecter (injection) where

import Tracy.Types

import Text.Parsec.String
import Text.Parsec.Char
import Text.Parsec.Prim (many, try, runP, parse)
import Text.Parsec.Combinator hiding (parse,parseFromFile)
import Data.List (intercalate)
import Data.String (fromString)
-- import qualified Text.Parsec.Token as P
-- import qualified Text.Parsec.Char as PC
-- import Text.Parsec.Prim (runP)
-- import qualified Text.ParserCombinators.Parsec.Expr as E

-------------------------------------------------------------------------------
-- ** Main functions
-------------------------------------------------------------------------------

injection :: Settings -> IO String
injection settings = 
  do lines <- parseInput $ filename settings
     let injLines = injectLine (injections settings) lines
         blockLines = injectBlock (blockAnnotation settings) injLines
     return $ intercalate "\n" blockLines

-- Parsing input file

parseInput :: String -> IO [String]
parseInput filename = 
    do result <- parseFromFile parser filename
       case result of
                  Left err  -> error $ show err
                  Right xs  -> return xs
    where
        parser =
            do l <- many $ try (manyTill anyChar newline)
               c <- many anyChar
               return $ l ++ [c]

--- Injection Blocks

injectBlock :: [(String,String)] -> [String] -> [String]
injectBlock blockann lines = foldl addBlocks lines parsedAnn
  where
    parsedAnn :: [(String, (Int,Int))]
    parsedAnn = map (\(x,y) -> (x, parseBlockInterval y)) blockann

addBlocks :: [String] -> (String, (Int,Int)) -> [String]
addBlocks list (blockname, (from, to)) = 
  front ++ (begin:err) ++ (end:back)
  where
    begin = "// BEGIN(" ++ blockname ++ ")"
    end   = "// END(" ++ blockname ++ ")"
    (front,rest) = splitAt (from - 1) list
    (err,back) = splitAt (to - from +1) rest

parseBlockInterval :: String -> (Int, Int)
parseBlockInterval str =
       case parse biParser "Block Annotation" str of
          Left err      -> error $ show err
          Right (n1,n2) -> 
            if n1 > n2
              then error $ "In block annotation: interval has negative size (" ++ show n1 ++ "-" ++ show n2 ++ ")"
              else (n1,n2)
    where
        biParser =
            do n1 <- number
               char '-'
               n2 <- number
               return (read n1, read n2)
        number = many digit

--- Injecting lines 

injectLine :: [String] -> [String] -> [String]
injectLine updates lines = foldl updateLine lines $ map parseInjection updates

updateLine :: [String] -> (Int, String) -> [String]
updateLine list (n, element) = (take (n-1) list) ++ (element:(drop n list))

parseInjection :: String -> (Int, String)
parseInjection str = 
       case parse injParser "Injections" str of
          Left err  -> error $ show err
          Right xs  -> xs
    where
        injParser =
            do n <- number
               char ':'
               s <- many anyChar
               return (read n,s)
        number = many digit

