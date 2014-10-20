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
-- |Basic types for the tracer
--
-----------------------------------------------------------------------------

module Tracy.Types where

import Tracy.Ast

-------------------------------------------------------------------------------
-- ** Setting for the tracer
-------------------------------------------------------------------------------

data Settings = Settings { filename      :: String
                         , function      :: String
                         , assignment    :: InputType
                         , int_width     :: Int
                         , config        :: String
                         , precondition  :: String
                         , postcondition :: String
                         , repairVar     :: Maybe String
                         , strip_out     :: [String]
                         , outputFormat  :: OutputFormat
                         , ppOutFile     :: Maybe String
                         , envFile       :: Either String Environment
                         , procSSA       :: Bool 
                         , proc3AC       :: Bool 
                         , debugMode     :: Bool
                         , varSize       :: VarSize
                         }
                deriving (Eq, Show)

emptySettings :: Settings
emptySettings = Settings { filename      = "main.c"
                         , function      = "main"
                         , assignment    = InputRandom
                         , int_width     = 32
                         , config        = ""
                         , precondition  = "true"
                         , postcondition = "true"
                         , repairVar     = Nothing
                         , strip_out     = []
                         , outputFormat  = Z3Int
                         , envFile       = Left ""
                         , ppOutFile     = Nothing
                         , proc3AC       = False
                         , procSSA       = False
                         , debugMode     = False
                         , varSize       = VarSize{slong = 64, sint = 32, sshort = 16, schar = 8}
                         }

data OutputFormat = Z3Int | Z3Bit | Ccode
                  deriving (Eq, Show)

data InputType    = InputValues String [Value] | InputRandom | InputZero
                  deriving (Eq, Show)

data Environment = Environment { envInt :: [Int] }
                  deriving (Eq, Show)

data VarSize = VarSize { slong  :: Int
                       , sint   :: Int
                       , sshort :: Int
                       , schar  :: Int
                       }
          deriving (Eq, Show)


-- importSettings :: Filename -> Settings
-- mergeSettings :: Settings -> Settings -> Settings

