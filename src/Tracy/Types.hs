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

data Settings = Settings { filename        :: String
                         , function        :: String
                         , assignment      :: InputType
                         , config          :: String
                         , precondition    :: String
                         , postcondition   :: String
                         , repairVar       :: Maybe String
                         , strip_out       :: [String]
                         , outputFormat    :: OutputFormat
                         , ppOutFile       :: Maybe String
                         , envFile         :: Either String Environment
                         , procSSA         :: Bool 
                         , proc3AC         :: Bool 
                         , debugMode       :: Bool
                         , linenumbers     :: Bool
                         , varSize         :: VarSize
                         , injections      :: [String]
                         , blockAnnotation :: [(String,String)]
                         }
                deriving (Eq, Show)

emptySettings :: Settings
emptySettings = Settings { filename        = "main.c"
                         , function        = "main"
                         , assignment      = InputRandom
                         , config          = ""
                         , precondition    = "true"
                         , postcondition   = "true"
                         , repairVar       = Nothing
                         , strip_out       = []
                         , outputFormat    = SMT2_QF_NIA
                         , envFile         = Left ""
                         , ppOutFile       = Nothing
                         , proc3AC         = False
                         , procSSA         = False
                         , debugMode       = False
                         , linenumbers     = False
                         , varSize         = VarSize{slong = 64, sint = 32, sshort = 16, schar = 8}
                         , injections      = []
                         , blockAnnotation = []
                         }

data OutputFormat = SMT2_QF_NIA | SMT2_QF_BV | Ccode
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

