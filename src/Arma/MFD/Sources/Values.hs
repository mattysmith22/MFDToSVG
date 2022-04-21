{-# LANGUAGE RecordWildCards, DeriveGeneric #-}
module Arma.MFD.Sources.Values where

import           Arma.MFD.Sources.Key
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Text (Text)
import           Data.Aeson
import           Arma.MFD
import           Data.Maybe (fromMaybe)
import           GHC.Generics (Generic)

data SourceValues = SourceValues {
    floatValues :: Map FloatSource Double,
    stringValues :: Map StringSource Text
}
    deriving (Show, Ord, Eq, Generic)

instance FromJSON SourceValues
instance ToJSON SourceValues
instance Semigroup SourceValues where
    l <> r = SourceValues {
        floatValues = floatValues l <> floatValues r,
        stringValues = stringValues l <> stringValues r
    }

instance Monoid SourceValues where
    mempty = SourceValues mempty mempty

mkSourceValues
    :: Map FloatSource Double 
    -> Map StringSource Text
    -> SourceValues
mkSourceValues = SourceValues

getFloatValue :: SourceValues -> FloatSource -> Double 
getFloatValue SourceValues{..} = fromMaybe 0 . flip Map.lookup floatValues

getStringValue :: SourceValues -> StringSource -> Text
getStringValue SourceValues{..} = fromMaybe "" . flip Map.lookup stringValues
