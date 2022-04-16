{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}
module Arma.MFD.Sources where

import           Arma.MFD
import           Arma.SimpleExpression
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.Read                as TR

boolSources :: Set.Set Text
boolSources = Set.fromList ["on"]

floatSources :: Set.Set Text
floatSources = Set.fromList ["altitudeAGL"]

readSimpleExpressionSource :: Text -> Maybe (Either BoolSource FloatSource)
readSimpleExpressionSource (T.stripPrefix "user" -> Just num) =
  case TR.decimal num of
    (Left  err     ) -> Nothing
    (Right (val, _)) -> Just $ Right $ FloatSourceUser val
readSimpleExpressionSource name
  | name `Set.member` boolSources  = Just $ Left $ BoolSource name
  | name `Set.member` floatSources = Just $ Right $ FloatSource name
  | otherwise                      = Nothing

type SourceReqs
  = (Set.Set FloatSource, Set.Set StringSource, Set.Set BoolSource)

addFloatReq :: FloatSource -> SourceReqs -> SourceReqs
addFloatReq source (f, s, b) = (source `Set.insert` f, s, b)

addStringReq :: StringSource -> SourceReqs -> SourceReqs
addStringReq source (f, s, b) = (f, source `Set.insert` s, b)

addBoolReq :: BoolSource -> SourceReqs -> SourceReqs
addBoolReq source (f, s, b) = (f, s, source `Set.insert` b)

simpleExpressionAddSources :: SimpleExpression -> SourceReqs -> SourceReqs
simpleExpressionAddSources expr req =
  foldl addReq req $ Set.map readSimpleExpressionSource $ getRequiredIdents expr
 where
  addReq set Nothing              = set
  addReq set (Just (Right float)) = float `addFloatReq` set
  addReq set (Just (Left  bool )) = bool `addBoolReq` set

colorAddSources :: Color -> SourceReqs -> SourceReqs
colorAddSources (r, g, b, a) =
  simpleExpressionAddSources r
    . simpleExpressionAddSources g
    . simpleExpressionAddSources b
    . simpleExpressionAddSources a

boneAddSources :: MFDBone -> SourceReqs -> SourceReqs
boneAddSources Fixed{}         = id
boneAddSources Linear {..}     = addFloatReq boneFloatSource
boneAddSources Rotational {..} = addFloatReq boneFloatSource
boneAddSources Horizon{}       = id

mfdElementAddSources :: MFDElement -> SourceReqs -> SourceReqs
mfdElementAddSources MFDElementLine{}    = id
mfdElementAddSources MFDElementText {..} = case mfdElementSource of
  (Left  _     ) -> id
  (Right source) -> addStringReq source
mfdElementAddSources MFDElementPolygon{}  = id
mfdElementAddSources MFDElementGroup {..} = \src ->
  let groupSources =
        addOrDefaultCol mfdElementColor
          $ addOrDefault mfdElementAlpha
          $ addOrDefault mfdElementCondition src

      addOrDefaultCol mCol = case mCol of
        (Just col) -> colorAddSources col
        Nothing    -> id

      addOrDefault mExpr = case mExpr of
        (Just expr) -> simpleExpressionAddSources expr
        Nothing     -> id
  in  foldl (flip mfdElementAddSources) groupSources mfdElementChildren


mfdGetSources :: MFD -> SourceReqs
mfdGetSources (MFD color bones draw) =
  colorAddSources color
    $ flip (foldl (flip boneAddSources)) (fmap snd bones)
    $ mfdElementAddSources draw mempty 
