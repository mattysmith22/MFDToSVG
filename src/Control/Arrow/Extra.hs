module Control.Arrow.Extra(maybeA, maybeA') where

import Prelude hiding ((.), id)
import Control.Category
import Control.Arrow
import Data.Either.Extra (fromEither)

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