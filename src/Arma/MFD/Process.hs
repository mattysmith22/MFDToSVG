{-# LANGUAGE TupleSections, RecordWildCards, Rank2Types, Arrows, ScopedTypeVariables #-}
module Arma.MFD.Process(process, ProcessingContext(..)) where
import Data.Text (Text)
import Arma.MFD
import Arma.MFD.Sources.With
import Data.Map (Map)
import Prelude hiding ((.), id)
import Control.Category
import Vec
import Control.Arrow
import qualified Data.Map as Map
import Arma.SimpleExpression.Eval (evalSimpleExpression')
import Data.Maybe (fromMaybe)
import Control.Arrow.Extra

type BoneMap = Map Text (Vec2 -> Vec2)

flipVertically :: Vec2 -> Vec2
flipVertically (V2 x y) = V2 x (-y)

calcBone :: MFDBone -> WithSource (Vec2 -> Vec2)
calcBone (Fixed coord) = WithSource $ arr $ const (^+^ coord)
calcBone Linear{..} = WithSource $ proc _ -> do
    val <- getFloat boneFloatSource -< ()
    let val' = clamp boneSourceMin boneSourceMax (val * boneSourceScale)
        addVec = interpVec (boneSourceMin, boneSourceMax) (bonePosMin, bonePosMax) val'
    returnA -< (^+^ addVec)
calcBone Rotational{..} = WithSource $ proc _ -> do
    val <- getFloat boneFloatSource -< ()
    let val' = clamp boneSourceMin boneSourceMax val
        val'' = interpSingle (boneSourceMin, boneSourceMax) (boneRotMin, boneRotMax) val'
    returnA -< (^+^ boneCenter) . rotate' val'' . flipVertically
-- TODO: Implement horizon
calcBone Horizon{..} = WithSource $ proc _ -> do
    bank <- getFloat (FloatSource "horizonbank") -< ()
    dive <- getFloat (FloatSource "horizondive") -< ()
    let elev = - (boneHorizonElevation + dive)
    returnA -< (^+^ bonePos0) . liftI2 (*) (V2 aspect 1) . rotate' bank . (^+^ V2 0 (elev * size))  . (^* (0.5 * sqrt 2))
    where
        (V2 x0 y0) = bonePos0
        (V2 x10 y10) = bonePos10
        size = (y10 - y0) / 10
        aspect = (x0 - x10) / (y0 - y10)

calcBones :: [(Text, MFDBone)] -> WithSource BoneMap
calcBones  x = Map.fromList <$> traverse calcBone' x
    where
        calcBone' (k,x) = (k,) <$> calcBone x

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
            in boneVal . (mfdPointTransformOffset ^+^) 

calcElement :: ProcessingContext -> UnProcessed MFDElement -> (forall arr. WithSources arr => arr BoneMap (Processed MFDElement))
calcElement _ MFDElementLine{..} = proc boneMap -> do
    let newPoints = fmap (fmap (calcPoint boneMap)) mfdElementPoints
    returnA -< MFDElementLine{mfdElementPoints = newPoints, ..}
calcElement _ MFDElementText{..} = proc boneMap -> do
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
calcElement _ MFDElementPolygon{..} = proc boneMap -> do
    let newPoints = fmap (fmap (calcPoint boneMap)) mfdElementPoints
    returnA -< MFDElementPolygon{mfdElementPoints = newPoints, ..}
calcElement ctx MFDElementGroup{..} =  proc boneMap -> do
    children <- traverseA (calcElement ctx) mfdElementChildren -< boneMap
    col <- maybeA' (unWithSource . calcColor) mfdElementColor-< ()
    alpha <- maybeA' (unWithSource . evalSimpleExpression') mfdElementAlpha -< ()
    cond <- maybeA' (unWithSource . evalSimpleExpression') mfdElementCondition -< ()
    returnA -< MFDElementGroup
        { mfdElementChildren = children
        , mfdElementColor = col
        , mfdElementAlpha = alpha
        , mfdElementCondition = cond
        , ..}
calcElement ctx@ProcessingContext{..} MFDElementPylon{..} = proc boneMap -> do
        matchingElements <- Map.foldrWithKey f id possiblePylons -< []
        let transform vec = calcPoint boneMap (mfdElementPos ++ [MFDPointTransform Nothing vec 1])
        let matchingElements' = fmap (mapPointElement transform) matchingElements
        returnA -< MFDElementGroup
            { mfdElementName = mfdElementName
            , mfdElementChildren = matchingElements'
            , mfdElementColor = Nothing
            , mfdElementClip = Nothing
            , mfdElementAlpha = Nothing
            , mfdElementCondition = Nothing
            }
    where
        possiblePylons :: Map Text (UnProcessed PylonMFD)
        possiblePylons = fromMaybe mempty $ Map.lookup mfdElementPylonType pylonsContext

        f :: forall arr. WithSources arr => Text -> UnProcessed PylonMFD -> arr [Processed MFDElement] [Processed MFDElement] -> arr [Processed MFDElement] [Processed MFDElement]
        f name pylon accArr = proc acc -> do
            pylonMag <- getString (StringSourcePylonMag mfdElementPylonIndex) -< ()
            if pylonMag == name then do
                processedPylon <- underPylon mfdElementPylonIndex $ unWithSource $ processPylon ctx pylon -< ()
                accArr -< processedPylon:acc
            else
                accArr -< acc

newtype ProcessingContext = ProcessingContext {
    pylonsContext :: Map Text (Map Text (UnProcessed PylonMFD))
}

processPylon :: ProcessingContext -> UnProcessed PylonMFD -> WithSource (Processed MFDElement)
processPylon ctx PylonMFD{..} = WithSource $ proc _ -> do
    boneMap <- unWithSource $ calcBones pylonBones -< ()
    calcElement ctx pylonDraw -< boneMap

process :: ProcessingContext -> UnProcessed MFD -> WithSource (Processed MFD)
process ctx MFD{..} = WithSource $ proc _ -> do
    boneMap <- unWithSource $ calcBones bones -< ()
    rootCol <- unWithSource $ calcColor color -< ()
    element <- calcElement ctx draw -< boneMap
    returnA -< MFD
        { color = rootCol
        , bones = ()
        , draw = element
        , ..
        }