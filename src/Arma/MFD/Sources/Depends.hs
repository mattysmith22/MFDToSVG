{-# LANGUAGE FlexibleInstances #-}
module Arma.MFD.Sources.Depends(SourceDeps, getSourceDependencies) where

import           Arma.MFD
import           Arma.SimpleExpression
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Control.Category
import           Control.Arrow
import           Prelude hiding ((.), id)
import           Arma.MFD.Sources.With

type SourceDeps
  = (Set.Set FloatSource, Set.Set StringSource, Set.Set BoolSource)

addFloatReq :: FloatSource -> SourceDeps -> SourceDeps
addFloatReq source (f, s, b) = (source `Set.insert` f, s, b)

addStringReq :: StringSource -> SourceDeps -> SourceDeps
addStringReq source (f, s, b) = (f, source `Set.insert` s, b)

addBoolReq :: BoolSource -> SourceDeps -> SourceDeps
addBoolReq source (f, s, b) = (f, s, source `Set.insert` b)

newtype SourceDepArr a b = SourceDepArr {
  runSourceDepArr :: SourceDeps -> SourceDeps
}

instance Category SourceDepArr where
  l . r = SourceDepArr $ runSourceDepArr l . runSourceDepArr r
  id = SourceDepArr id

instance Arrow SourceDepArr where
  arr _ = SourceDepArr id
  
  l *** r = SourceDepArr $ runSourceDepArr l . runSourceDepArr r

instance ArrowChoice SourceDepArr where
  l +++ r = SourceDepArr $ runSourceDepArr l . runSourceDepArr r

instance WithSources SourceDepArr where
  getFloat src = SourceDepArr $ addFloatReq src
  getString src = SourceDepArr $ addStringReq src
  getBool src = SourceDepArr $ addBoolReq src

getSourceDependencies :: WithSource a -> SourceDeps
getSourceDependencies (WithSource a) = runSourceDepArr a mempty
