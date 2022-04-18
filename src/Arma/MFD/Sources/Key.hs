{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Arma.MFD.Sources.Key where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import           Arma.MFD
import           Data.Aeson
import           Data.Aeson.Encoding.Internal (text)

class SourceKey k where
    toSourceKey :: k -> Text
    fromSourceKey :: Text -> k

instance SourceKey FloatSource where
    toSourceKey (FloatSource x) = x
    toSourceKey (FloatSourceUser x) = "user" <> T.pack (show x)

    fromSourceKey x = case T.stripPrefix "user" x of
        Nothing -> FloatSource x
        (Just suf) -> case TR.decimal suf of
            (Right (n,"")) -> FloatSourceUser n
            _ -> FloatSource x

instance SourceKey StringSource where
    toSourceKey (StringSource x) = x
    toSourceKey (StringSourceTime x) = "time#" <> x
    toSourceKey (StringSourceUser x) = "user" <> T.pack (show x)

    fromSourceKey x = case T.stripPrefix "user" x of
        (Just suf) -> case TR.decimal suf of
            (Right (n,"")) -> StringSourceUser n
            _ -> StringSource x
        Nothing ->  case T.stripPrefix "time#" x of
            (Just suf) -> StringSourceTime suf
            Nothing -> StringSource x

instance SourceKey BoolSource where
    toSourceKey (BoolSource x) = x

    fromSourceKey x = BoolSource x

instance SourceKey a => ToJSONKey a where
    toJSONKey = ToJSONKeyText toSourceKey (text . toSourceKey)
    toJSONKeyList = ToJSONKeyText (T.intercalate "," . fmap toSourceKey) (text . T.intercalate "," . fmap toSourceKey)

instance SourceKey a => FromJSONKey a where
    fromJSONKey = FromJSONKeyText fromSourceKey
    fromJSONKeyList = FromJSONKeyText (fmap fromSourceKey . T.splitOn ",")