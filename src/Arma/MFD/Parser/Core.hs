module Arma.MFD.Parser.Core where

import Arma.Config.Parser

data MFDParserError = InvalidType String
    | InvalidColor
    deriving (Eq, Show)

type Parser a = ConfigParser MFDParserError a