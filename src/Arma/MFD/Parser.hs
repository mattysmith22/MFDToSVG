
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
import Arma.MFD.Parser.Element

-- | Parse a full MFD from the config position
parseMfd :: Parser MFD
parseMfd = do
    color <- parseColor "color"
    bones <- onSubConfig "Bones" $ onSubConfigs' (\ident -> (ident,) <$> parseBone)
    elements <- onSubConfig "Draw" parseGroup
    return (MFD color bones elements)