{-# LANGUAGE Arrows, TupleSections, RecordWildCards, Rank2Types #-}
module Arma.MFD.Draw where

import           Arma.MFD.Sources.With
import           Arma.MFD
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Map as Map
import Arma.SimpleExpression.Eval
import Vec
import Arma.MFD.Bone (applyBone)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Functor (void)
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Arrow.Extra
import Control.Category
import Control.Applicative

type BoneMap = Map Text (Vec2 -> Vec2)

calcBones :: [(Text, MFDBone)] -> WithSource BoneMap
calcBones  x = Map.fromList <$> traverse calcBone x
    where
        calcBone (k,x) = (k,) <$> applyBone x

calcColor :: Color -> WithSource (Double, Double, Double, Double)
calcColor (r,g,b,a) = (,,,)
    <$> evalSimpleExpression' r
    <*> evalSimpleExpression' g
    <*> evalSimpleExpression' b
    <*> evalSimpleExpression' a

calcPoint :: BoneMap -> MFDPoint -> Vec2
calcPoint boneMap = foldr calcTransform (V2 0 0)
    where
        --TODO: Add weight
        calcTransform :: MFDPointTransform -> (Vec2 -> Vec2)
        calcTransform MFDPointTransform{..} = let
            boneVal = maybe id (fromMaybe id . (`Map.lookup` boneMap)) mfdPointTransformBone 
            in (mfdPointTransformOffset ^+^) . boneVal

drawElement :: MFDElement  -> (forall arr. WithSources arr => arr BoneMap ())
drawElement MFDElementLine{..} = arr $ const ()
drawElement MFDElementText{..} = either (const $ arr $ const ()) (\x -> arr (const ()) . getString x) mfdElementSource
drawElement MFDElementPolygon{..} = arr $ const ()
drawElement MFDElementGroup{..} = proc boneMap -> do
    children <- unwrapArrow $ traverse_ (WrapArrow . drawElement) mfdElementChildren -< boneMap
    maybeA' (unWithSource . calcColor) mfdElementColor -< () 
    maybeA' (unWithSource . evalSimpleExpression') mfdElementCondition -< ()
    arr (const ()) -< ()

drawMFD :: MFD -> WithSource ()
drawMFD MFD{..} = WithSource $ proc _ -> do
    bones' <- unWithSource $ calcBones bones -< ()
    _rootCol <- unWithSource $ calcColor color -< ()
    drawElement draw -< bones'