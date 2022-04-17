{-# LANGUAGE RecordWildCards, Arrows #-}
module Arma.MFD.Bone where
import Arma.MFD
import Vec
import Arma.MFD.Sources.With
import Control.Arrow

applyBone :: MFDBone -> WithSource (Vec2 -> Vec2)
applyBone (Fixed coord) = WithSource $ arr $ const (^+^ coord)
applyBone Linear{..} = WithSource $ proc _ -> do
    val <- getFloat boneFloatSource -< ()
    let val' = clamp boneSourceMin boneSourceMax val
        addVec = interpVec (boneSourceMin, boneSourceMax) (bonePosMin, bonePosMax) val'
    returnA -< (^+^ addVec)
applyBone Rotational{..} = WithSource $ proc _ -> do
    val <- getFloat boneFloatSource -< ()
    let val' = clamp boneSourceMin boneSourceMax val
        val'' = interpSingle (boneSourceMin, boneSourceMax) (boneRotMin, boneRotMax) val'
    returnA -< rotateAround val'' boneCenter
-- TODO: Implement horizon
applyBone Horizon{..} = WithSource $ arr $ const id