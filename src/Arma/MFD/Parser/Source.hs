
{-|
Module      : Arma.MFD.Parser.Source
Description : Parser for MFD sources
-}
module Arma.MFD.Parser.Source where

import Arma.Config.Parser
import Arma.MFD
import Data.Char
import Arma.MFD.Parser.Types
import qualified Data.Text as T

-- | Reads a float source from the config
readFloatSource :: ConfigParser u FloatSource
readFloatSource = do
    sourceType <- readString "source"
    case T.toLower sourceType of
        "user" ->
            FloatSourceUser . truncate <$> readNumber "sourceIndex"
        typ -> 
            return $ FloatSource typ

-- | Reads a string source from the config
readStringSource :: ConfigParser u StringSource
readStringSource = do
    sourceType <- readString "source"
    case T.toLower sourceType of
        "userText" -> do
            StringSourceUser . truncate <$> readNumber "sourceIndex"
        "time" -> 
            StringSourceTime <$> readString "text"
        "static" ->
            StringSourceStatic <$> readString "text"
        typ -> return $ StringSource typ