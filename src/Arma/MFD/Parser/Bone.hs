
{-|
Module      : Arma.Config.Parser
Description : Parses MFD bones from config
-}
module Arma.MFD.Parser.Bone (parseBone) where

import Arma.Config.Parser
import Arma.MFD.Parser.Source
import Arma.MFD.Parser.Types
import Arma.MFD
import Data.Char

-- |Parses a single bone at the parser's position
parseBone :: Parser MFDBone
parseBone = do
    boneType <- readString "type"
    case fmap toLower boneType of
        "fixed" -> parseFixedBone
        "linear" -> parseLinearBone
        "rotational" -> parseRotationalBone
        "horizon" -> parseHorizonBone
        invalidType -> userConfigError (InvalidType invalidType) ["type"]

parseFixedBone :: Parser MFDBone
parseFixedBone = Fixed <$> readVec2 "pos"

parseLinearBone :: Parser MFDBone
parseLinearBone = do
    source <- readFloatSource
    sourceMin <- readNumber "min"
    sourceMax <- readNumber "max"
    sourceScale <- wDefault 1 $ readNumber "sourceScale"
    posMin <- readVec2 "maxPos" -- minPos matches max source value - for historical reasons
    posMax <- readVec2 "minPos"
    return Linear {
        boneFloatSource = source,
        boneSourceMin = sourceMin,
        boneSourceMax = sourceMax,
        boneSourceScale = sourceScale,
        bonePosMin = posMin,
        bonePosMax = posMax
    }

parseRotationalBone :: Parser MFDBone
parseRotationalBone = do
    source <- readFloatSource
    sourceMin <- readNumber "min"
    sourceMax <- readNumber "max"
    sourceScale <- wDefault 1 $ readNumber "sourceScale"
    minAngle <- readNumber "minAngle"
    maxAngle <- readNumber "maxAngle"
    return Rotational {
        boneFloatSource = source,
        boneSourceMin  = sourceMin,
        boneSourceMax = sourceMax,
        boneSourceScale = sourceScale,
        boneRotMin = minAngle,
        boneRotMax = maxAngle
    }

parseHorizonBone :: Parser MFDBone
parseHorizonBone = do
    elevation <- readNumber "angle"
    pos0 <- readVec2 "pos0"
    pos10 <- readVec2 "pos10"
    return Horizon {
        boneHorizonElevation = elevation,
        bonePos0 = pos0,
        bonePos10 = pos10
    }