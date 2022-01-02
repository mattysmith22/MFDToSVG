{-|
Module      : Arma.MFD
Description : Types relating to MFD elements.

For more information on these values and what they represent, please see

https://community.bistudio.com/wiki/Arma_3:_Multi-Function_Display_(MFD)_config_reference
-}
module Arma.MFD where

import           Arma.SimpleExpression
import           Arma.Value
import           Data.Text                      ( Text )

-- | A vector of two arma numbers
type Vec2 = (ArmaNumber, ArmaNumber)

-- | Full MFD representation
data MFD = MFD
  { color :: Color
  , bones :: [(Text, MFDBone)]
  , draw  :: MFDElement
  }
  deriving (Show, Eq)

-- | Sum type of all available bones
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

-- | Valid float source
data FloatSource = FloatSource Text
    | FloatSourceUser Int
    deriving (Show, Eq, Ord)

-- | Valid string source
data StringSource = StringSource Text
    | StringSourceTime Text
    | StringSourceUser Int
    deriving (Show, Eq, Ord)

newtype BoolSource = BoolSource Text
    deriving (Show, Eq, Ord)

-- | A point on the MFD, defined as a series of transformations
type MFDPoint = [MFDPointTransform]

-- | A single transformation to be performed on a point
data MFDPointTransform = MFDPointTransform
  { mfdPointTransformBone   :: Maybe Text
  , mfdPointTransformOffset :: Vec2
  , mfdPointTransformWeight :: ArmaNumber
  }
  deriving (Show, Eq)

-- | A single color
type Color
  = (SimpleExpression, SimpleExpression, SimpleExpression, SimpleExpression)

-- | Valid line values for (type="line")
data LineType = LineTypeFull | LineTypeDotted | LineTypeDashed | LineTypeDotDashed
    deriving (Show, Eq)

-- | Valid text alignment values (type="text")
data TextAlign = TextAlignLeft | TextAlignRight | TextAlignCenter
    deriving (Show, Eq)

-- | Valid view modes modes for (type="scale") (showing North, East, South, West) instead of numbers
data ScaleModeNESW = ScaleModeNESWNone -- ^ Don't show any NESW text.
    | ScaleModeNESWPart -- ^ Show N,E,S,W
    | ScaleModeNESWFull -- ^ Show N,NE,E,SE,S,SW,W,NW
    deriving (Show, Eq)

-- | Sum type of all MFD elements
data MFDElement = MFDElementLine {
        mfdElementPoints :: [[MFDPoint]],
        mfdElementWidth :: ArmaNumber,
        mfdElementLineType :: LineType
    }
    | MFDElementText {
        mfdElementAlign :: TextAlign,
        mfdElementScale :: ArmaNumber,
        mfdElementSource :: Either Text StringSource,
        mfdElementSourceScale :: ArmaNumber,
        mfdElementSourceLength :: Maybe ArmaNumber,
        mfdElementSourcePrecision :: Maybe ArmaNumber,
        mfdElementTextPos :: MFDPoint,
        mfdElementTextRight :: MFDPoint,
        mfdElementTextDown :: MFDPoint
    }
    | MFDElementPolygon {
        mfdElementPoints :: [[MFDPoint]]
    }
    {-| MFDElementScale {
        mfdElementAlign :: TextAlign,
        mfdElementScale :: ArmaNumber,
        mfdElementNesw :: ScaleModeNESW,
        mfdElementFloatSource :: FloatSource,
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
    }-}
    | MFDElementGroup {
        mfdElementChildren :: [MFDElement],
        mfdElementColor :: Maybe Color,
        mfdElementAlpha :: Maybe SimpleExpression,
        mfdElementClip :: Maybe (Vec2, Vec2),
        mfdElementCondition :: Maybe SimpleExpression
    }
    deriving (Show, Eq)
