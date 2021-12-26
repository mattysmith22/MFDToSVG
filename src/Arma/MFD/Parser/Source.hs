module Arma.MFD.Parser.Source where

import Arma.Config.Parser
import Arma.MFD
import Data.Char
import Arma.MFD.Parser.Core

readFloatSource :: ConfigParser u FloatSource
readFloatSource = do
    sourceType <- readString "source"
    case fmap toLower sourceType of
        "user" ->
            FloatSourceUser . truncate <$> readNumber "sourceIndex"
        typ -> 
            return $ FloatSource typ

readStringSource :: ConfigParser u StringSource
readStringSource = do
    sourceType <- readString "source"
    case fmap toLower sourceType of
        "userText" -> do
            StringSourceUser . truncate <$> readNumber "sourceIndex"
        "time" -> 
            StringSourceTime <$> readString "text"
        "static" ->
            StringSourceStatic <$> readString "text"
        typ -> return $ StringSource typ