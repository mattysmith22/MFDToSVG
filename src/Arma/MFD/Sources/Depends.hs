{-# LANGUAGE FlexibleInstances, TupleSections #-}
module Arma.MFD.Sources.Depends(SourceDeps, getSourceDependencies, defaultValues) where

import           Arma.MFD
import           Arma.SimpleExpression
import qualified Data.Set                      as Set
import qualified Data.Map                      as Map
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Category
import           Control.Arrow
import           Prelude hiding ((.), id)
import           Arma.MFD.Sources.With
import           Arma.MFD.Sources.Values

type SourceDeps
  = (Set.Set FloatSource, Set.Set StringSource, Set.Set BoolSource)

addFloatReq :: FloatSource -> SourceDeps -> SourceDeps
addFloatReq source (f, s, b) = (source `Set.insert` f, s, b)

addStringReq :: StringSource -> SourceDeps -> SourceDeps
addStringReq source (f, s, b) = (f, source `Set.insert` s, b)

addBoolReq :: BoolSource -> SourceDeps -> SourceDeps
addBoolReq source (f, s, b) = (f, s, source `Set.insert` b)

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
  getBool src = SourceDepArr $ \mInd deps -> flip addBoolReq deps $ maybe id BoolSourcePylon mInd src
  underPylon n x = SourceDepArr $ const $ runSourceDepArr' x (Just n)

getSourceDependencies :: WithSource a -> SourceDeps
getSourceDependencies (WithSource a) = runSourceDepArr a mempty

defaultValues :: SourceDeps -> SourceValues
defaultValues (floatDeps, stringDeps, boolDeps) = SourceValues {
    floatValues = toMap floatDeps 0,
    stringValues = toMap stringDeps "",
    boolValues = toMap boolDeps False
}
  where
      toMap deps defVal = Map.fromList $ map (,defVal) $ Set.toList deps