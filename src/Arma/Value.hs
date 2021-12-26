module Arma.Value where

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as MC
import qualified Text.Megaparsec.Char.Lexer as MCL


type ArmaNumber = Double 
data ArmaType = ArmaTypeString | ArmaTypeNumber | ArmaTypeArray
    deriving (Show, Eq)

data ArmaValue = ArmaString String
    | ArmaNumber ArmaNumber
    | ArmaArray [ArmaValue]
    deriving (Show, Eq)

getArmaType :: ArmaValue -> ArmaType
getArmaType (ArmaArray _) = ArmaTypeArray
getArmaType (ArmaString _) = ArmaTypeString
getArmaType (ArmaNumber _) = ArmaTypeNumber