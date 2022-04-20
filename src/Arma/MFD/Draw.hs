{-# LANGUAGE TupleSections, RecordWildCards, Rank2Types #-}
module Arma.MFD.Draw (drawMFD) where

import           Arma.MFD.Sources.With
import           Arma.MFD
import           Data.Map (Map)
import           Data.Text (Text)
import qualified Data.Map as Map
import qualified Data.Text as T
import Arma.SimpleExpression.Eval
import Vec
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

col :: RawColor -> Text
col (r,g,b,a) = "rgba(" <> T.intercalate "," (fmap asPct [r,g,b,a]) <> ")"
    where
        asPct = (<> "%") . T.pack . show . clamp 0 100 . (*100)

lA' :: V2 Double -> Text
lA' (V2 x y) = lA x y

mA' :: V2 Double -> Text 
mA' (V2 x y) = mA x y

textCoord :: V2 Double -> [Attribute]
textCoord (V2 x y) = [X_ <<- T.pack (show x), Y_ <<- T.pack  (show y)]

path' :: [V2 Double] -> Text
path' [] = undefined
path' (m:ls) = mA' m <> foldMap lA' ls

strokeWidthFudgeFactor :: Double 
strokeWidthFudgeFactor = 0.001

textDrawFudgeFactor :: V2 Double 
textDrawFudgeFactor = V2 0.15 0.15

drawElement :: Double -> RawColor -> Processed MFDElement -> Fresh Element
drawElement scale c MFDElementLine{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toLine $ filter ((> 1) . length) mfdElementPoints
    where
        strokeWidth = strokeWidthFudgeFactor * scale * mfdElementWidth

        toLine :: [Vec2] -> Element
        toLine points = path_ [Stroke_width_ <<- T.pack (show strokeWidth), Stroke_ <<- col c, Fill_ <<- "transparent", D_ <<- path' points]
-- TODO: Draw text
drawElement scale c MFDElementText{..} = let
        height = norm (mfdElementTextPos ^-^ mfdElementTextDown)
        dim = T.pack (show height) <> "px"

        anchor = case mfdElementAlign of
            TextAlignLeft -> "end"
            TextAlignCenter -> "middle"
            TextAlignRight -> "start"

        fudgeMultiplier = case mfdElementAlign of
            TextAlignLeft -> V2 1 1
            TextAlignCenter -> V2 0 1
            TextAlignRight -> V2 (-1) 1

        fudge = textDrawFudgeFactor `mulBy` (fudgeMultiplier ^* height)

        element = text_
            ([Fill_ <<- col c, Font_family_ <<- "Ticketing", Dominant_baseline_ <<- "hanging", Text_anchor_ <<- anchor, Font_size_ <<- dim, makeAttribute "data-name" mfdElementName] ++ textCoord (mfdElementTextPos ^+^ fudge))
            (toElement mfdElementSource)
    in pure element
drawElement _ c MFDElementPolygon{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toPoly $ filter ((>1) . length) mfdElementPoints
    where
        toPoly :: [Vec2] -> Element
        toPoly points = path_ [Fill_ <<- col c, D_ <<- path' points]

drawElement scale c MFDElementGroup{..} = let
    c' = fromMaybe c mfdElementColor
    shouldShow = fromMaybe 1 mfdElementCondition > 0
    in if shouldShow then
        g_ [makeAttribute "data-name" mfdElementName] <$> foldMap (drawElement scale c') mfdElementChildren
    else
        pure $ g_ [makeAttribute "data-name" mfdElementName, makeAttribute "data-group-empty" "true"]

svg :: V2 Double -> Element -> Element
svg (V2 x y) content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- T.pack (show x), Height_ <<- T.pack (show y)]

withFont :: String -> Element -> Element
withFont fontPath = (fontDef <>)
    where
        fontDef = defs_ [] $ style_ [] $ toElement $ "@font-face {font-family: Ticketing; src: url(" <> T.pack fontPath <> ");}"

withBlackBackground :: V2 Double -> Element -> Element 
withBlackBackground (V2 x y) = (bg <>)
    where
        bg = path_ [Fill_<<- "black", D_ <<- (mA' (V2 0 0) <> lA' (V2 0 y) <> lA' (V2 x y) <> lA' (V2 x 0))]

mulBy :: (Additive f, Num a) => f a -> f a -> f a
mulBy = liftU2 (*)

drawMFD' :: Double -> Processed MFD -> Element
drawMFD' scale MFD{..} = runFresh $ drawElement scale color draw

drawMFD :: String -> V2 Double -> Processed MFD -> Element
drawMFD fontPath size@(V2 _ y) = svg size . withFont fontPath . withBlackBackground size . drawMFD' y . fmap (mulBy size)
