module Vec
    ( module Linear
    , Vec2
    , rotateAround
    , rotate'
    , interpVec
    , interpSingle
    , clamp
    ) where

import Linear

type Vec2 = V2 Double

deg2rad :: Double -> Double 
deg2rad = (/ 180) . (* pi)

rotateAround :: Double -> Vec2 -> Vec2 -> Vec2
rotateAround t centre = (^+^centre) . rotate' t . (^-^centre)

rotate' :: Double -> Vec2 -> Vec2
rotate' t (V2 x y) = V2
    (x * cos t' - y * sin t')
    (x * sin t' + y * cos t')
    where
        t' = deg2rad t

interpVec :: (Double,Double) -> (Vec2, Vec2) -> Double -> Vec2
interpVec (from,to) (fromV, toV) inp = lerp x' fromV toV
    where
        x' = clamp 0 1 x
        x = (inp -  from) / (to - from)

clamp :: Ord a => a -> a -> a -> a
clamp a b = min (a `max` b) . max (a `min` b)

interpSingle :: (Ord a, Fractional a) => (a, a) -> (a, a) -> a -> a
interpSingle (fromIn, toIn) (fromOut, toOut) inp =
        fromOut + ((toOut - fromOut) * x')
    where
        x' = clamp 0 1 x
        x = (inp -  fromIn) / (toIn - fromIn)