{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Arma.MFD.Sources.Key (SourceKey(..)) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Arma.MFD
import           Data.Aeson
import           Data.Aeson.Encoding.Internal (text)
import           Control.Applicative
import Data.Maybe (fromMaybe)

class SourceKey k where
    toSourceKey :: k -> Text
    fromSourceKey :: Text -> k

rightToMaybe :: Either a b -> Maybe b
rightToMaybe = either (const Nothing) Just

parsePylonPrefix :: SourceKey a => Text -> Text -> Maybe (Int, a)
parsePylonPrefix pref x = do
    (rawNum, suff) <- T.breakOn "$" <$> T.stripPrefix pref x
    (num,_) <- rightToMaybe $ TR.decimal rawNum
    return (num, fromSourceKey suff)

parseUserPrefix :: Text -> Text -> Maybe Int
parseUserPrefix pref x = T.stripPrefix pref x >>= rightToMaybe . fmap fst .  TR.decimal

instance SourceKey FloatSource where
    toSourceKey (FloatSource x) = x
    toSourceKey (FloatSourceUser x) = "user" <> T.pack (show x)
    toSourceKey (FloatSourcePylon n x) = "$pylon" <> T.pack (show n) <> "$" <> toSourceKey x

    fromSourceKey x = fromMaybe (FloatSource x) $
            (uncurry FloatSourcePylon <$> parsePylonPrefix "$pylon" x)
        <|> (FloatSourceUser <$> parseUserPrefix "user" x)
instance SourceKey StringSource where
    toSourceKey (StringSource x) = x
    toSourceKey (StringSourceTime x) = "time$" <> x
    toSourceKey (StringSourceUser x) = "user" <> T.pack (show x)
    toSourceKey (StringSourcePylon n x) = "$pylon" <> T.pack (show n) <> "$" <> toSourceKey x
    toSourceKey (StringSourcePylonMag n) = "$pylonmag" <> T.pack (show n)

    fromSourceKey x = fromMaybe (StringSource x) $
            (StringSourcePylonMag <$> parseUserPrefix "$pylonmag" x)
        <|> (uncurry StringSourcePylon <$> parsePylonPrefix "$pylon" x)
        <|> (StringSourceUser <$> parseUserPrefix "user" x)
        <|> (StringSourceTime <$> T.stripPrefix "time$" x)
instance SourceKey BoolSource where
    toSourceKey (BoolSource x) = x
    toSourceKey (BoolSourcePylon n x) = "$pylon" <> T.pack (show n) <> "$" <> toSourceKey x

    fromSourceKey x = maybe (BoolSource x) (uncurry BoolSourcePylon) $ parsePylonPrefix "$pylon" x


instance SourceKey a => ToJSONKey a where
    toJSONKey = ToJSONKeyText toSourceKey (text . toSourceKey)
    toJSONKeyList = ToJSONKeyText (T.intercalate "," . fmap toSourceKey) (text . T.intercalate "," . fmap toSourceKey)

instance SourceKey a => FromJSONKey a where
    fromJSONKey = FromJSONKeyText fromSourceKey
    fromJSONKeyList = FromJSONKeyText (fmap fromSourceKey . T.splitOn ",")