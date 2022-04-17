{-# LANGUAGE GADTs, Rank2Types, ScopedTypeVariables #-}
module Arma.MFD.Sources.With where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Arma.MFD
import Arma.MFD.Sources.Values
import Data.Text (Text)

class Arrow a => WithSources a where
    getFloat :: FloatSource -> a b Double 
    getString :: StringSource -> a b Text
    getBool :: BoolSource  -> a b Bool

newtype SourceArr a b = SourceArr {
    runSourceArr :: SourceValues -> a -> b
}

instance Category SourceArr where
    l . r = SourceArr $ \vals -> runSourceArr l vals . runSourceArr r vals

    id = SourceArr $ const id

instance Arrow SourceArr where
    arr = SourceArr . const

    lArr *** rArr = SourceArr $ \vals (l, r) ->
        (runSourceArr lArr vals l, runSourceArr rArr vals r)

instance WithSources SourceArr where
    getFloat src = SourceArr $ \vals _ -> getFloatValue vals src
    getString src = SourceArr $ \vals _ -> getStringValue vals src
    getBool src = SourceArr $ \vals _ -> getBoolValue vals src

data WithSource a where
    WithSource :: (forall arr. WithSources arr => arr () a) -> WithSource a

runWithSource :: forall a. WithSource a -> SourceValues -> a
runWithSource (WithSource arr) vals = runSourceArr arr vals ()

unWithSource :: WithSource a -> (forall arr. WithSources arr => arr () a)
unWithSource (WithSource arr) = arr