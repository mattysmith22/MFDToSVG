{-# LANGUAGE GADTs, Rank2Types, ScopedTypeVariables, LambdaCase #-}
module Arma.MFD.Sources.With where

import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Arma.MFD
import Arma.MFD.Sources.Values
import Data.Text (Text)

class ArrowChoice a => WithSources a where
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

instance ArrowChoice SourceArr where
    l +++ r = SourceArr $ \vals -> \case
        (Left lVal) -> Left $ runSourceArr l vals lVal
        (Right rVal) -> Right $ runSourceArr r vals rVal
instance WithSources SourceArr where
    getFloat src = SourceArr $ \vals _ -> getFloatValue vals src
    getString src = SourceArr $ \vals _ -> getStringValue vals src
    getBool src = SourceArr $ \vals _ -> getBoolValue vals src

data WithSource a where
    WithSource :: (forall arr. WithSources arr => arr () a) -> WithSource a

runWithSource :: WithSource a -> SourceValues -> a
runWithSource (WithSource arr) vals = runSourceArr arr vals ()

unWithSource :: WithSource a -> (forall arr. WithSources arr => arr () a)
unWithSource (WithSource arr) = arr

instance Functor WithSource where
  fmap f x = WithSource $ (arr f .) $ unWithSource x

instance Applicative WithSource where
    pure x = WithSource $ arr $ const x

    f <*> x = WithSource
        $ arr (uncurry ($))
        . (unWithSource f &&& unWithSource x)