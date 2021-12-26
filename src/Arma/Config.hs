module Arma.Config where

import Arma.Value
import Data.Map as Map
import Data.Map (Map)

data Config = Config {
    properties :: [(String, ArmaValue)],
    subClasses :: [(String, Config)]
}
    deriving (Show, Eq)

data ConfigError = InvalidConfigType
    | InvalidConfigLength
    | InvalidProperty
    | InvalidSubClass
    deriving (Show, Eq)

fromArmaValue :: ArmaValue -> Either ConfigError Config
fromArmaValue (ArmaArray [ArmaArray rawProperties, ArmaArray rawSubClasses]) = 
        Config <$> propertiesMapE <*> subClassesMapE
    where
        propertiesMapE = mapM parseProperty rawProperties

        parseProperty (ArmaArray [ArmaString key, value]) = Right (key, value)
        parseProperty _ = Left InvalidProperty

        subClassesMapE = mapM parseSubClass rawSubClasses

        parseSubClass (ArmaArray [ArmaString key, rawSubClass]) = do
            subClass <- fromArmaValue rawSubClass
            return (key, subClass)
        parseSubClass _ = Left InvalidSubClass
    
fromArmaValue (ArmaArray _) = Left InvalidConfigLength
fromArmaValue _ = Left InvalidConfigType 