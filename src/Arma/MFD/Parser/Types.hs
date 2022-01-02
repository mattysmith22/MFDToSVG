{-|
Module      : Arma.MFD.Parser.Types
Description : Base types for the MFD Parser
-}
module Arma.MFD.Parser.Types where

import           Arma.Config.Parser             ( ConfigParser
                                                , Tree(Leaf, Node)
                                                , readSimpleExpressionArray
                                                , userConfigError
                                                )
import           Arma.MFD
import           Arma.Value                     ( ArmaNumber )
import           Data.Text                      ( Text )

-- | Parses a color from a valid string
parseColor :: Text -> Parser Color
parseColor ident = do
  arr <- readSimpleExpressionArray ident
  case arr of
    Node [Leaf r, Leaf g, Leaf b, Leaf a] -> return (r, g, b, a)
    _ -> userConfigError InvalidColor [ident]

-- | Errors that may occur while parsing an MFD
data MFDParserError = InvalidType Text
    | InvalidColor
    | InvalidLineType ArmaNumber
    | InvalidPoint
    | InvalidTextAlign Text
    deriving (Eq, Show)

-- | Parser type
type Parser a = ConfigParser MFDParserError a
