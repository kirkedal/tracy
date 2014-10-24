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

module Tracy.LineInjecter (injectLine) where

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

injectLine :: String -> [String] -> IO String
injectLine filename updates =
    do lines <- parseInput filename
       let pUpdates = map parseInjection updates
       return $ intercalate "\n" $ foldl updateLine lines pUpdates

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

