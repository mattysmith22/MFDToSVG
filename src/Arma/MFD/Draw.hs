{-# LANGUAGE Arrows, TupleSections, RecordWildCards, Rank2Types #-}
module Arma.MFD.Draw where

import           Arma.MFD.Sources.With
import           Arma.MFD
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import Arma.SimpleExpression.Eval
import Vec
import Arma.MFD.Bone (applyBone)
import Data.Foldable
import Data.Maybe (fromMaybe)
import Data.Functor (void)
import Prelude hiding ((.), id)
import Control.Arrow
import Control.Arrow.Extra
import Control.Category ( Category((.), id) )
import Control.Applicative
import Graphics.Svg

newtype Fresh a = Fresh {
    runFresh' :: Int -> (Int, a)
}

instance Functor Fresh where
    fmap f x = Fresh $ second f . runFresh' x
instance Applicative Fresh where
    pure x = Fresh (,x)
    af <*> ax = Fresh $ \i -> let 
        (i', f) = runFresh' af i
        (i'', x) = runFresh' ax i'
        in (i'', f x)
instance Semigroup a => Semigroup (Fresh a) where
    l <> r = (<>) <$> l <*> r
instance Monoid a => Monoid (Fresh a) where
    mempty = pure mempty

runFresh :: Fresh a -> a
runFresh = snd . flip runFresh' 0

fresh :: Fresh Int
fresh = Fresh $ \i -> (i+1, i)

type BoneMap = Map Text (Vec2 -> Vec2)

type RawColor = (Double,Double,Double,Double)

col :: RawColor -> Text
col (r,g,b,a) = "rgba(" <> T.intercalate "," (fmap asPct [r,g,b,a]) <> ")"
    where
        asPct = (<> "%") . T.pack . show . clamp 0 100 . (*100)

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

lA' :: V2 Double -> Text
lA' (V2 x y) = lA (x*100) (y*100)

mA' :: V2 Double -> Text 
mA' (V2 x y) = mA (x*100) (y*100)

textCoord :: V2 Double -> [Attribute]
textCoord (V2 x y) = [X_ <<- T.pack (show (x * 100)), Y_ <<- T.pack  (show (y * 100))]

path' :: [V2 Double] -> Text
path' [] = undefined
path' (m:ls) = mA' m <> foldMap lA' ls

drawElement :: MFDElement  -> (forall arr. WithSources arr => arr (RawColor, BoneMap) (Fresh Element))
drawElement MFDElementLine{..} = arr (\vals -> pure $ g_ [makeAttribute "data-name" mfdElementName] $ foldMap (toLine vals) $ filter (not. null) mfdElementPoints)
    where
        toLine :: (RawColor, BoneMap) -> [MFDPoint] -> Element
        toLine (c, boneMap) points = path_ [Stroke_width_ <<- "0.1", Stroke_ <<- col c, Fill_ <<- "transparent", D_ <<- path' (fmap (calcPoint boneMap) points)]
-- TODO: Draw text
drawElement MFDElementText{..} = proc (c,boneMap)  -> do
    sourceVal <- either (arr . const) getString mfdElementSource -< ()

    let pos = calcPoint boneMap mfdElementTextPos
        right = calcPoint boneMap mfdElementTextRight
        down = calcPoint boneMap mfdElementTextDown
    
    let height = norm (pos ^-^ down) * 0.75
    let dim = T.pack (show (height * 100)) <> "px"
    let anchor = case mfdElementAlign of
            TextAlignLeft -> "end"
            TextAlignCenter -> "middle"
            TextAlignRight -> "start"

    let element = text_
            ([Fill_ <<- col c, Font_family_ <<- "Consolas", Dominant_baseline_ <<- "hanging", Text_anchor_ <<- anchor, Font_size_ <<- dim, makeAttribute "data-name" mfdElementName] ++ textCoord pos)
            (toElement sourceVal)
    returnA -< pure element
drawElement MFDElementPolygon{..} = arr (\vals -> pure $ g_ [makeAttribute "data-name" mfdElementName] $ foldMap (toPoly vals) $ filter (not. null) mfdElementPoints)
    where
        toPoly :: (RawColor, BoneMap) -> [MFDPoint] -> Element
        toPoly (c, boneMap) points = path_ [Fill_ <<- col c, D_ <<- path' (calcPoint boneMap <$> points)]

drawElement MFDElementGroup{..} = proc (c, boneMap) -> do
    mNewColor <- maybeA' (unWithSource . calcColor) mfdElementColor -< ()
    let c' = fromMaybe c mNewColor 
    maybeA' (unWithSource . evalSimpleExpression') mfdElementCondition -< ()
    arr (fmap (g_ [makeAttribute "data-name" mfdElementName])) . foldMapA drawElement mfdElementChildren -< (c', boneMap)

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100", Height_ <<- "100"]

withBlackBackground :: Element -> Element 
withBlackBackground = (bg <>)
    where
        bg = path_ [Fill_<<- "black", D_ <<- (mA' (V2 0 0) <> lA' (V2 0 1) <> lA' (V2 1 1) <> lA' (V2 1 0))]

drawMFD :: MFD -> WithSource (Fresh Element)
drawMFD MFD{..} = WithSource $ proc _ -> do
    bones' <- unWithSource $ calcBones bones -< ()
    rootCol <- unWithSource $ calcColor color -< ()
    arr (fmap (svg . withBlackBackground)) . drawElement draw -< (rootCol, bones')