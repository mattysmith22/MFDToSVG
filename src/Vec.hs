module Vec where

-- | A vector of two arma numbers
type Vec2 = (Double, Double)

(#+#) :: Vec2 -> Vec2 -> Vec2
(x1,y1) #+# (x2,y2) = (x1 + x2, y1 + y2)

(#-#) :: Vec2 -> Vec2 -> Vec2
(x1,y1) #-# (x2,y2) = (x1 - x2, y1 - y2)

rotateAround :: Double -> Vec2 -> Vec2 -> Vec2
rotateAround t centre = (#+#centre) . rotate t . (#-#centre)

rotate :: Double -> Vec2 -> Vec2
rotate t (x,y) = (x * cos t - y * sin t, x * sin t + y * cos t)

interpVec :: (Double,Double) -> (Vec2, Vec2) -> Double -> Vec2
interpVec (from,to) ((x1,y1), (x2,y2)) inp =
    ( interpSingle (from,to) (x1,y1) inp
    , interpSingle (from,to) (x2,y2) inp
    )

clamp :: Ord a => a -> a -> a -> a
clamp a b = min (a `max` b) . max (a `min` b)

interpSingle :: (Ord a, Fractional a) => (a, a) -> (a, a) -> a -> a
interpSingle (fromIn, toIn) (fromOut, toOut) inp = clamp fromOut toOut $
        fromOut + ((toOut - fromOut) * x)
    where
        x = (inp -  fromIn) / (toIn - fromIn)