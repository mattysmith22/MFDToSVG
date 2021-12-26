module Arma.MFD where

import Arma.Value
import Arma.SimpleExpression

type Vec2 = (ArmaNumber, ArmaNumber)

data MFD = MFD {
    color :: Color,
    bones :: [(String, MFDBone)],
    draw :: MFDElement
}
    deriving (Show, Eq)

data MFDBone = Fixed Vec2
    | Linear {
        boneFloatSource :: FloatSource,
        boneSourceMin :: ArmaNumber,
        boneSourceMax :: ArmaNumber,
        boneSourceScale :: ArmaNumber,
        bonePosMin :: Vec2,
        bonePosMax :: Vec2
    }
    | Rotational {
        boneFloatSource :: FloatSource,
        boneSourceMin :: ArmaNumber,
        boneSourceMax :: ArmaNumber,
        boneSourceScale :: ArmaNumber,
        boneRotMin :: ArmaNumber,
        boneRotMax :: ArmaNumber
    }
    | Horizon {
        boneHorizonElevation :: ArmaNumber,
        bonePos0 :: Vec2,
        bonePos10 :: Vec2
    }
    deriving (Show, Eq)

data FloatSource = FloatSource String 
    | FloatSourceUser Int
    deriving (Show, Eq)

data StringSource = StringSource String
    | StringSourceTime String 
    | StringSourceUser Int
    | StringSourceStatic String
    deriving (Show, Eq)

type MFDPoint = [MFDPointTransform]

data MFDPointTransform = MFDPointTransform {
    mfdPointTransformBone :: Maybe String,
    mfdPointTransformOffset :: Maybe Vec2,
    mfdPointTransformWeight :: ArmaNumber
}
    deriving (Show, Eq)

type Color = (SimpleExpression, SimpleExpression, SimpleExpression, SimpleExpression)

data LineType = LineTypeFull | LineTypeDotted | LineTypeDashed | LineTypeDotDashed
    deriving (Show, Eq)
data TextAlign = TextAlignLeft | TextAlignRight | TextAlignCenter
    deriving (Show, Eq)
data ScaleModeNESW = ScaleModeNESWNone | ScaleModeNESWPart | ScaleModeNESWFull
    deriving (Show, Eq)

data MFDElement = MFDElementLine {
        mfdElementPoints :: [[MFDPoint]],
        mfdElementWidth :: ArmaNumber,
        mfdElementLineType :: LineType
    }
    | MFDElementText {
        mfdElementAlign :: TextAlign,
        mfdElementScale :: ArmaNumber,
        mfdElementSource :: StringSource,
        mfdElementSourceScale :: ArmaNumber,
        mfdElementSourceLength :: ArmaNumber,
        mfdElementSourcePrecision :: ArmaNumber,
        mfdElementPos :: Vec2,
        mfdElementRight :: Vec2,
        mfdElementDown :: Vec2
    }
    | MFDElementPolygon {
        mfdElementPoints :: [[MFDPoint]]
    }
    | MFDElementScale {
        mfdElementAlign :: TextAlign,
        mfdElementScale :: ArmaNumber,
        mfdElementNesw :: ScaleModeNESW,
        mfdElementSourceScale :: ArmaNumber,
        mfdElementHorizontal :: Bool,
        mfdElementPos :: Vec2,
        mfdElementRight :: Vec2,
        mfdElementDown :: Vec2,
        mfdElementStep :: ArmaNumber,
        mfdElementStepSize :: ArmaNumber,
        mfdElementLine :: (ArmaNumber, ArmaNumber),
        mfdElementLineMajor :: (ArmaNumber, ArmaNumber),
        mfdElementTop :: ArmaNumber,
        mfdElementBottom :: ArmaNumber,
        mfdElementCenter :: ArmaNumber,
        mfdElementMajorLineEach :: Int,
        mfdElementNumberEach :: Int,
        mfdElementMin :: ArmaNumber,
        mfdElementMax :: ArmaNumber
    }
    | MFDElementGroup {
        mfdElementChildren :: [MFDElement],
        mfdElementColor :: Maybe Color,
        mfdElementAlpha :: Maybe SimpleExpression,
        mfdElementClip :: Maybe (Vec2, Vec2),
        mfdElementCondition :: Maybe SimpleExpression
    }
    deriving (Show, Eq)