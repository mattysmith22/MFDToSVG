{-# LANGUAGE RecordWildCards #-}
module Arma.MFD.Bone where
import Arma.MFD
import Vec
import Arma.MFD.Sources.With


applyBone :: MFDBone -> WithSource (Vec2 -> Vec2)
applyBone (Fixed coord) = WithSource $ arr (#+# coord)
applyBone (Rotational {..}) -> WithSource