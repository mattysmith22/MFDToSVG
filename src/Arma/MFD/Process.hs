{-# LANGUAGE TupleSections, RecordWildCards, Rank2Types, Arrows #-}
module Arma.MFD.Process(process) where
import Data.Text (Text)
import Arma.MFD
import Arma.MFD.Sources.With
import Data.Map (Map)
import Vec
import Control.Arrow
import qualified Data.Map as Map
import Arma.MFD.Bone (applyBone)
import Arma.SimpleExpression.Eval (evalSimpleExpression')
import Data.Maybe (fromMaybe)
import Control.Arrow.Extra

type BoneMap = Map Text (Vec2 -> Vec2)

calcBones :: [(Text, MFDBone)] -> WithSource BoneMap
calcBones  x = Map.fromList <$> traverse calcBone x
    where
        calcBone (k,x) = (k,) <$> applyBone x

calcColor :: Color -> WithSource RawColor
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

calcElement :: UnProcessed MFDElement -> (forall arr. WithSources arr => arr BoneMap (Processed MFDElement))
calcElement MFDElementLine{..} = proc boneMap -> do
    let newPoints = fmap (fmap (calcPoint boneMap)) mfdElementPoints
    returnA -< MFDElementLine{mfdElementPoints = newPoints, ..}
calcElement MFDElementText{..} = proc boneMap -> do
    let pos = calcPoint boneMap mfdElementTextPos
    let right = calcPoint boneMap mfdElementTextRight
    let down = calcPoint boneMap mfdElementTextDown
    
    sourceVal <- evalSource mfdElementSource -< ()

    returnA -< MFDElementText
        { mfdElementTextPos = pos
        , mfdElementTextRight = right
        , mfdElementTextDown = down
        , mfdElementSource = sourceVal
        , ..}
    where
        evalSource (Left raw) = arr (const raw)
        evalSource (Right src) = getString src
calcElement MFDElementPolygon{..} = proc boneMap -> do
    let newPoints = fmap (fmap (calcPoint boneMap)) mfdElementPoints
    returnA -< MFDElementPolygon{mfdElementPoints = newPoints, ..}
calcElement MFDElementGroup{..} =  proc boneMap -> do
    children <- traverseA calcElement mfdElementChildren -< boneMap
    col <- maybeA' (unWithSource . calcColor) mfdElementColor-< ()
    alpha <- maybeA' (unWithSource . evalSimpleExpression') mfdElementAlpha -< ()
    cond <- maybeA' (unWithSource . evalSimpleExpression') mfdElementCondition -< ()
    returnA -< MFDElementGroup
        { mfdElementChildren = children
        , mfdElementColor = col
        , mfdElementAlpha = alpha
        , mfdElementCondition = cond
        , ..}

process :: UnProcessed MFD -> WithSource (Processed MFD)
process MFD{..} = WithSource $ proc _ -> do
    boneMap <- unWithSource $ calcBones bones -< ()
    rootCol <- unWithSource $ calcColor color -< ()
    element <- calcElement draw -< boneMap
    returnA -< MFD
        { color = rootCol
        , bones = ()
        , draw = element
        }