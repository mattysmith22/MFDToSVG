
{-|
Module      : Arma.MFD.Parser
Description : Parses a full MFD from config.
-}
{-#LANGUAGE TupleSections#-}
module Arma.MFD.Parser
  ( parseMfd
  , parsePylonMfd
  , MFDParserError(..)
  , Parser
  ) where

import           Arma.Config.Parser
import           Arma.MFD
import           Arma.MFD.Parser.Bone
import           Arma.MFD.Parser.Element
import           Arma.MFD.Parser.Types

-- | Parse a full MFD from the config position
parseMfd :: Parser (UnProcessed MFD)
parseMfd = do
  color <- parseColor "color"
  bones <- onSubConfig "Bones"
    $ onSubConfigs' (\ident -> (ident, ) <$> parseBone)
  font <- readString "font"
  elements <- onSubConfig "Draw" (parseGroup "<root>")
  return (MFD color bones font elements)

parsePylonMfd :: Parser (UnProcessed PylonMFD)
parsePylonMfd = do
  bones <- onSubConfig "Bones"
    $ onSubConfigs' (\ident -> (ident, ) <$> parseBone)
  elements <- onSubConfig "Draw" (parseGroup "<root>")
  return (PylonMFD bones elements)