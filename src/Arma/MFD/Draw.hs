{-# LANGUAGE TupleSections, RecordWildCards, Rank2Types #-}
module Arma.MFD.Draw (drawMFD, DrawContext(..)) where

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

data DrawContext = DrawContext
    { mfdSize :: V2 Double
    , armaFontMappings :: Map Text Text
    , newFontMappings :: [(Text, [(Text, Text)])]
    }

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
textDrawFudgeFactor = V2 0.3 0.15

drawElement :: DrawContext -> Text -> RawColor -> Processed MFDElement -> Fresh Element
drawElement ctx _ c MFDElementLine{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toLine $ filter ((> 1) . length) mfdElementPoints
    where
        strokeWidth = strokeWidthFudgeFactor * mfdScaling ctx * mfdElementWidth

        toLine :: [Vec2] -> Element
        toLine points = path_ [Stroke_width_ <<- T.pack (show strokeWidth), Stroke_ <<- col c, Fill_ <<- "transparent", D_ <<- path' points]
-- TODO: Draw text
drawElement DrawContext{..} font c MFDElementText{..} = let
        height = norm (mfdElementTextPos ^-^ mfdElementTextDown)
        widthVec = norm (mfdElementTextPos ^-^ mfdElementTextRight)
        aspect = widthVec / height
        dim = T.pack (show height) <> "px"

        anchor = case mfdElementAlign of
            TextAlignLeft -> "end"
            TextAlignCenter -> "middle"
            TextAlignRight -> "start"

        fudgeMultiplier = case mfdElementAlign of
            TextAlignLeft -> V2 0 1
            TextAlignCenter -> V2 0 1
            TextAlignRight -> V2 0 1

        fudge = textDrawFudgeFactor `mulBy` (fudgeMultiplier ^* height)

        fontName = fromMaybe font $ Map.lookup font armaFontMappings

        transform = "scale(" <> T.pack (show aspect) <> ", 1)"

        --The "scale() transform also affects the coordinate, so we need to counteract that"
        finalPos = mulBy (V2 (recip aspect) 1) $ mfdElementTextPos + fudge
        element = text_
            ([Fill_ <<- col c, Font_family_ <<- fontName
                , Dominant_baseline_ <<- "hanging", Text_anchor_ <<- anchor
                , Style_ <<- "white-space: pre; text-rendering: geometricPrecision"
                , Font_size_ <<- dim, Transform_ <<- transform
                , makeAttribute "data-name" mfdElementName]
            ++ textCoord finalPos)

            (toElement (T.replace " " "\x00A0\x00A0" mfdElementSource)) --Replace with NBSP to prevent them being removed
    in pure element
drawElement _ _ c MFDElementPolygon{..} = pure $ g_ [makeAttribute "data-name" mfdElementName] $
        foldMap toPoly $ filter ((>1) . length) mfdElementPoints
    where
        toPoly :: [Vec2] -> Element
        toPoly points = path_ [Fill_ <<- col c, D_ <<- path' points]

drawElement ctx font c MFDElementGroup{..} = let
    c' = fromMaybe c mfdElementColor
    shouldShow = fromMaybe 1 mfdElementCondition > 0
    in if shouldShow then
        g_ [makeAttribute "data-name" mfdElementName] <$> (maybe (pure id) withClip mfdElementClip <*> foldMap (drawElement ctx font c') mfdElementChildren)
    else
        pure $ g_ [makeAttribute "data-name" mfdElementName, makeAttribute "data-group-empty" "true"]
    where
        withClip :: (Vec2, Vec2) -> Fresh (Element -> Element)
        withClip (bl, tr) = (\fid children -> clipPath_ [Id_ <<- fid] (path_ [D_ <<- rectToPath bl tr]) <> g_ [Style_ <<- "clip-path:url(#" <> fid <> ")"] children) . ("clip" <>) . T.pack . show <$> fresh
drawElement _ _ _ MFDElementPylon{..} = error "Cannot draw MFDElementPylon, this should have been processed into a MFDElementGroup"

rectToPath :: Vec2 -> Vec2 -> Text
rectToPath (V2 x1 y1) (V2 x2 y2) = mA' (V2 x1 y1) <> lA' (V2 x1 y2) <> lA' (V2 x2 y2) <> lA' (V2 x2 y1) <> lA' (V2 x1 y1)

svg :: DrawContext -> Element -> Element
svg DrawContext{mfdSize = (V2 x y)} content =
     doctype
  <> with (svg11_ content) [Version_ <<- "1.1", Width_ <<- T.pack (show x), Height_ <<- T.pack (show y)]

withFonts :: DrawContext -> Element -> Element
withFonts DrawContext{..} = (fontDef <>)
    where
        fontDef = defs_ [] $ style_ [] $ toElement $ T.intercalate " " $ fmap fontDef' newFontMappings

        fontDef' :: (Text, [(Text, Text)]) -> Text
        fontDef' (name, defs) = "@font-face {font-family: " <> name <> "; src: " <> T.intercalate ", " (fmap formatUrl defs) <> ";}"

        formatUrl :: (Text, Text) -> Text
        formatUrl (url, format) = "url(\"" <> url <> "\") format(\"" <> format <> "\")"

withBlackBackground :: DrawContext -> Element -> Element 
withBlackBackground DrawContext{..} = (bg <>)
    where
        bg = path_ [Fill_<<- "black", D_ <<- rectToPath (V2 0 0) mfdSize]

mulBy :: (Additive f, Num a) => f a -> f a -> f a
mulBy = liftU2 (*)

mfdScaling :: DrawContext -> Double
mfdScaling DrawContext{ mfdSize = V2 _ y} = y

drawMFD' :: DrawContext -> Processed MFD -> Element
drawMFD' ctx MFD{..} = runFresh $ drawElement ctx font color draw

drawMFD :: DrawContext -> Processed MFD -> Element
drawMFD ctx = svg ctx . withFonts ctx . withBlackBackground ctx . drawMFD' ctx . mapPoint (mulBy (mfdSize ctx))