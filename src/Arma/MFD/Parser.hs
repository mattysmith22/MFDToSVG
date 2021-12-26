
{-|
Module      : Arma.MFD.Parser
Description : Parses a full MFD from config.
-}
{-#LANGUAGE TupleSections#-}
module Arma.MFD.Parser (parseMfd, MFDParserError(..), Parser) where

import Arma.Config.Parser
import Arma.MFD
import Arma.MFD.Parser.Types
import Arma.MFD.Parser.Bone

parseColor :: String -> Parser Color
parseColor ident = do
    arr <- readSimpleExpressionArray ident
    case arr of
        Node [Leaf r, Leaf g, Leaf b, Leaf a] -> return (r,g,b,a)
        _ -> userConfigError InvalidColor [ident]

-- | Parse a full MFD from the config position
parseMfd :: Parser MFD
parseMfd = do
    color <- parseColor "color"
    bones <- onSubConfig "Bones" $ onSubConfigs' (\ident -> (ident,) <$> parseBone)
    return (MFD color bones $ MFDElementGroup [] Nothing Nothing Nothing Nothing)