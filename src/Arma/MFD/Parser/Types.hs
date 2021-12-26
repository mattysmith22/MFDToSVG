{-|
Module      : Arma.MFD.Parser.Types
Description : Base types for the MFD Parser
-}
module Arma.MFD.Parser.Types where

import Arma.Config.Parser

-- | Errors that may occur while parsing an MFD
data MFDParserError = InvalidType String
    | InvalidColor
    deriving (Eq, Show)

-- | Parser type
type Parser a = ConfigParser MFDParserError a