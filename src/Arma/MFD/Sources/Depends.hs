{-# LANGUAGE FlexibleInstances, TupleSections, DeriveGeneric #-}
module Arma.MFD.Sources.Depends(SourceDeps(..), getSourceDependencies, defaultValues) where

import           Arma.MFD
import           Arma.SimpleExpression
import           Data.Aeson
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Category
import           Control.Arrow
import           Prelude hiding ((.), id)
import           Arma.MFD.Sources.With
import           Arma.MFD.Sources.Values
import           GHC.Generics

data SourceDeps = SourceDeps
  { floatDeps :: Set.Set FloatSource
  , stringDeps :: Set.Set StringSource
  }
  deriving Generic

instance Semigroup SourceDeps where
    l <> r = SourceDeps
      { floatDeps = floatDeps l <> floatDeps r
      , stringDeps = stringDeps l <> stringDeps r
      }

instance ToJSON SourceDeps where
    toEncoding = genericToEncoding defaultOptions

instance Monoid SourceDeps where
  mempty = SourceDeps mempty mempty

addFloatReq :: FloatSource -> SourceDeps -> SourceDeps
addFloatReq source x = x { floatDeps = source `Set.insert` floatDeps x}

addStringReq :: StringSource -> SourceDeps -> SourceDeps
addStringReq source x = x { stringDeps = source `Set.insert` stringDeps x}

newtype SourceDepArr a b = SourceDepArr {
  runSourceDepArr' :: Maybe Int -> SourceDeps -> SourceDeps
}

runSourceDepArr :: SourceDepArr a b -> SourceDeps -> SourceDeps
runSourceDepArr x = runSourceDepArr' x Nothing
instance Category SourceDepArr where
  l . r = SourceDepArr $ \mInd -> runSourceDepArr' l mInd . runSourceDepArr' r mInd
  id = SourceDepArr $ const id

instance Arrow SourceDepArr where
  arr _ = SourceDepArr $ const id
  
  l *** r = SourceDepArr $ \mInd -> runSourceDepArr' l mInd . runSourceDepArr' r mInd

instance ArrowChoice SourceDepArr where
  l +++ r = SourceDepArr $ \mInd -> runSourceDepArr' l mInd . runSourceDepArr' r mInd

instance WithSources SourceDepArr where
  getFloat src = SourceDepArr $ \mInd deps -> flip addFloatReq deps $ maybe id FloatSourcePylon mInd src
  getString src = SourceDepArr $ \mInd deps -> flip addStringReq deps $ maybe id StringSourcePylon mInd src
  underPylon n x = SourceDepArr $ const $ runSourceDepArr' x (Just n)

getSourceDependencies :: WithSource a -> SourceDeps
getSourceDependencies (WithSource a) = runSourceDepArr a mempty

defaultValues :: SourceDeps -> SourceValues
defaultValues x = SourceValues {
    floatValues = toMap (floatDeps x) 0,
    stringValues = toMap (stringDeps x) ""
}
  where
      toMap deps defVal = Map.fromList $ map (,defVal) $ Set.toList deps