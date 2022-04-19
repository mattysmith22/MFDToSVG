{-# LANGUAGE TupleSections, RecordWildCards, Rank2Types #-}
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

col :: RawColor -> Text
col (r,g,b,a) = "rgba(" <> T.intercalate "," (fmap asPct [r,g,b,a]) <> ")"
    where
        asPct = (<> "%") . T.pack . show . clamp 0 100 . (*100)

lA' :: V2 Double -> Text
lA' (V2 x y) = lA (x*100) (y*100)

mA' :: V2 Double -> Text 
mA' (V2 x y) = mA (x*100) (y*100)

textCoord :: V2 Double -> [Attribute]
textCoord (V2 x y) = [X_ <<- T.pack (show (x * 100)), Y_ <<- T.pack  (show (y * 100))]

path' :: [V2 Double] -> Text
path' [] = undefined
path' (m:ls) = mA' m <> foldMap lA' ls

drawElement :: RawColor -> Processed MFDElement -> Fresh Element
drawElement c MFDElementLine{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toLine $ filter ((> 1) . length) mfdElementPoints
    where
        toLine :: [Vec2] -> Element
        toLine points = path_ [Stroke_width_ <<- "0.1", Stroke_ <<- col c, Fill_ <<- "transparent", D_ <<- path' points]
-- TODO: Draw text
drawElement c MFDElementText{..} = let
        height = norm (mfdElementTextPos ^-^ mfdElementTextDown)
        dim = T.pack (show (height * 100)) <> "px"
        anchor = case mfdElementAlign of
            TextAlignLeft -> "end"
            TextAlignCenter -> "middle"
            TextAlignRight -> "start"

        element = text_
            ([Fill_ <<- col c, Font_family_ <<- "Ticketing", Dominant_baseline_ <<- "hanging", Text_anchor_ <<- anchor, Font_size_ <<- dim, makeAttribute "data-name" mfdElementName] ++ textCoord mfdElementTextPos)
            (toElement mfdElementSource)
    in pure element
drawElement c MFDElementPolygon{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toPoly $ filter ((>1) . length) mfdElementPoints
    where
        toPoly :: [Vec2] -> Element
        toPoly points = path_ [Fill_ <<- col c, D_ <<- path' points]

drawElement c MFDElementGroup{..} = let
    c' = fromMaybe c mfdElementColor
    in g_ [makeAttribute "data-name" mfdElementName] <$> foldMap (drawElement c') mfdElementChildren

svg :: Element -> Element
svg content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- "100", Height_ <<- "100"]

withFont :: Element -> Element
withFont = (fontDef <>)
    where
        fontDef = defs_ [] $ style_ [] "@font-face {font-family: Ticketing; src: url(file:///home/mbs/Downloads/ticketing/ticketing/TICKETING/Ticketing.ttf);}"

withBlackBackground :: Element -> Element 
withBlackBackground = (bg <>)
    where
        bg = path_ [Fill_<<- "black", D_ <<- (mA' (V2 0 0) <> lA' (V2 0 1) <> lA' (V2 1 1) <> lA' (V2 1 0))]

drawMFD :: Processed MFD -> Fresh Element
drawMFD MFD{..} = svg . withFont . withBlackBackground <$> drawElement color draw