{-|
Module      : Arma.MFD
Description : Types relating to MFD elements.

For more information on these values and what they represent, please see

https://community.bistudio.com/wiki/Arma_3:_Multi-Function_Display_(MFD)_config_reference
-}
{-# LANGUAGE RecordWildCards #-}
module Arma.MFD where

import           Arma.SimpleExpression
import           Arma.Value
import           Data.Text                      ( Text )
import           Vec
import           Control.Arrow

type UnProcessed x = x [(Text, MFDBone)] Color FloatSource (Either Text StringSource) SimpleExpression MFDPoint
type Processed x = x () RawColor ArmaNumber Text ArmaNumber Vec2

type RawColor = (Double,Double,Double,Double)

data PylonMFD bone col float str se p = PylonMFD
    { pylonBones :: bone
    , pylonDraw :: MFDElement bone col float str se p
    }

-- | Full MFD representation
data MFD bone col float str se p = MFD
  { color :: col
  , bones :: bone
  , font :: Text
  , draw  :: MFDElement bone col float str se p
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
        boneCenter :: Vec2,
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
    | FloatSourcePylon Int FloatSource
    deriving (Show, Eq, Ord)

-- | Valid string source
data StringSource = StringSource Text
    | StringSourceTime Text
    | StringSourceUser Int
    | StringSourcePylon Int StringSource
    | StringSourcePylonMag Int
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
data MFDElement bone col float str se p = MFDElementLine {
        mfdElementName :: Text,
        mfdElementPoints :: [[p]],
        mfdElementWidth :: ArmaNumber,
        mfdElementLineType :: LineType
    }
    | MFDElementText {
        mfdElementName :: Text,
        mfdElementAlign :: TextAlign,
        mfdElementScale :: ArmaNumber,
        mfdElementSource :: str,
        mfdElementSourceScale :: ArmaNumber,
        mfdElementSourceLength :: Maybe ArmaNumber,
        mfdElementSourcePrecision :: Maybe ArmaNumber,
        mfdElementTextPos :: p,
        mfdElementTextRight :: p,
        mfdElementTextDown :: p
    }
    | MFDElementPolygon {
        mfdElementName :: Text,
        mfdElementPoints :: [[p]]
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
        mfdElementLine :: Vec2,
        mfdElementLineMajor :: Vec2,
        mfdElementTop :: ArmaNumber,
        mfdElementBottom :: ArmaNumber,
        mfdElementCenter :: ArmaNumber,
        mfdElementMajorLineEach :: Int,
        mfdElementNumberEach :: Int,
        mfdElementMin :: ArmaNumber,
        mfdElementMax :: ArmaNumber
    }-}
    | MFDElementGroup {
        mfdElementName :: Text,
        mfdElementChildren :: [MFDElement bone col float str se p],
        mfdElementColor :: Maybe col,
        mfdElementAlpha :: Maybe se,
        mfdElementClip :: Maybe (Vec2, Vec2),
        mfdElementCondition :: Maybe se
    }
    | MFDElementPylon {
        mfdElementName :: Text,
        mfdElementPos :: p,
        mfdElementPylonIndex :: Int,
        mfdElementPylonType :: Text
    }
    deriving (Show, Eq)


mapPoint :: (Vec2 -> Vec2) -> Processed MFD  -> Processed MFD
mapPoint f MFD{..} = MFD{draw = mapPointElement f draw, ..}

mapPointElement :: (Vec2 -> Vec2) -> Processed MFDElement -> Processed MFDElement
mapPointElement f MFDElementLine{..} = MFDElementLine{mfdElementPoints = fmap (fmap f) mfdElementPoints, ..}
mapPointElement f MFDElementPolygon{..} = MFDElementPolygon{mfdElementPoints = fmap (fmap f) mfdElementPoints, ..}
mapPointElement f MFDElementGroup{..} = MFDElementGroup
    { mfdElementChildren = fmap (mapPointElement f) mfdElementChildren
    , mfdElementClip = (f *** f ) <$> mfdElementClip
    , ..
    }
mapPointElement f MFDElementText{..} = MFDElementText
    { mfdElementTextPos = f mfdElementTextPos
    , mfdElementTextRight = f mfdElementTextRight
    , mfdElementTextDown = f mfdElementTextDown
    , ..
    }
mapPointElement f MFDElementPylon{..} = MFDElementPylon
    { mfdElementPos = f mfdElementPos
    , ..
    }