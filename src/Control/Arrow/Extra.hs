module Control.Arrow.Extra(maybeA, maybeA', foldMapA, traverseA) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Either.Extra (fromEither)
import Control.Applicative

maybeA :: ArrowChoice arr => arr a b -> arr (Maybe a) (Maybe b)
maybeA a = arr fromEither . right a .  arr toEither
    where
        toEither Nothing = Left ()
        toEither (Just x) = Right x

        fromEither (Left _) = Nothing 
        fromEither (Right x) = Just x

maybeA' :: Arrow arr => (a -> arr b c) -> (Maybe a -> arr b (Maybe c))
maybeA' a Nothing = arr $ const Nothing 
maybeA' a (Just x) = arr Just . a x

traverseA :: (Traversable t, Arrow arr) => (c -> arr a b) -> t c -> arr a (t b)
traverseA x = unwrapArrow . traverse (WrapArrow . x)

foldMapA :: (Foldable t, Arrow arr, Monoid m) => (c -> arr a m) -> t c -> arr a m
foldMapA f = foldl f' (arr $ const mempty)
    where
        f' acc x = arr (uncurry (<>)) . (f x &&& acc)