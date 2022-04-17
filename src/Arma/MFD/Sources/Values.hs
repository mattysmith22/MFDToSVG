{-# LANGUAGE RecordWildCards #-}
module Arma.MFD.Sources.Values where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Text (Text)

import           Arma.MFD
import           Data.Maybe (fromMaybe)

data SourceValues = SourceValues {
    floatValues :: Map FloatSource Double,
    stringValues :: Map StringSource Text,
    boolValues :: Map BoolSource Bool
}

instance Semigroup SourceValues where
    l <> r = SourceValues {
        floatValues = floatValues l <> floatValues r,
        stringValues = stringValues l <> stringValues r,
        boolValues = boolValues l <> boolValues r
    }

instance Monoid SourceValues where
    mempty = SourceValues mempty mempty mempty

mkSourceValues
    :: Map FloatSource Double 
    -> Map StringSource Text
    -> Map BoolSource Bool
    -> SourceValues
mkSourceValues = SourceValues

getFloatValue :: SourceValues -> FloatSource -> Double 
getFloatValue SourceValues{..} = fromMaybe 0 . flip Map.lookup floatValues

getStringValue :: SourceValues -> StringSource -> Text
getStringValue SourceValues{..} = fromMaybe "" . flip Map.lookup stringValues

getBoolValue :: SourceValues -> BoolSource -> Bool
getBoolValue SourceValues{..} = fromMaybe False . flip Map.lookup boolValues
