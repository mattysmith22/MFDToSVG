{-|
Module      : Arma.Value
Description : Haskell instance or Arma value
-}
module Arma.Value
  ( ArmaNumber(..)
  , ArmaType(..)
  , ArmaValue(..)
  , getArmaType
  ) where

import           Data.Text                      ( Text )
import qualified Text.Megaparsec               as M
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as MCL

-- |Number imported from arma
type ArmaNumber = Double

-- |Type of given arma value
data ArmaType = ArmaTypeString | ArmaTypeNumber | ArmaTypeArray
    deriving (Show, Eq)

-- |Sum type of all valid arma values
data ArmaValue = ArmaString Text
    | ArmaNumber ArmaNumber
    | ArmaArray [ArmaValue]
    deriving (Show, Eq)

-- |Returns type of a given arma value
getArmaType :: ArmaValue -> ArmaType
getArmaType (ArmaArray  _) = ArmaTypeArray
getArmaType (ArmaString _) = ArmaTypeString
getArmaType (ArmaNumber _) = ArmaTypeNumber
