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
    underPylon :: Int -> a b c -> a b c

newtype SourceArr a b = SourceArr {
    runSourceArr' :: Maybe Int -> SourceValues -> a -> b
}

runSourceArr :: SourceArr a b -> SourceValues -> a -> b
runSourceArr x = runSourceArr' x Nothing

instance Category SourceArr where
    l . r = SourceArr $ \mInd vals -> runSourceArr' l mInd vals . runSourceArr' r mInd vals

    id = SourceArr $ const $ const id

instance Arrow SourceArr where
    arr = SourceArr . const . const

    lArr *** rArr = SourceArr $ \mInd vals (l, r) ->
        (runSourceArr' lArr mInd vals l, runSourceArr' rArr mInd vals r)

instance ArrowChoice SourceArr where
    l +++ r = SourceArr $ \mInd vals -> \case
        (Left lVal) -> Left $ runSourceArr' l mInd vals lVal
        (Right rVal) -> Right $ runSourceArr' r mInd vals rVal
instance WithSources SourceArr where
    getFloat src = SourceArr $ \mPylon vals x -> getFloatValue vals $ maybe id FloatSourcePylon mPylon src
    getString src = SourceArr $ \mPylon vals x -> getStringValue vals $ maybe id StringSourcePylon mPylon src
    getBool src = SourceArr $ \mPylon vals x -> getBoolValue vals $ maybe id BoolSourcePylon mPylon src
    underPylon n arr = SourceArr $ \_ vals x -> runSourceArr' arr (Just n) vals x

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