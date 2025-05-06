{-|
Module      : Arma.SimpleExpression
Description : Simple expression evaluation library
-}
module Arma.SimpleExpression where
import           Arma.Value
import qualified Data.Map                      as Map
import qualified Data.Set                      as Set
import           Data.Text                      ( Text )
import           Text.Megaparsec

type Ident = Text

data BinOp
    = OpMul
    | OpDiv
    | OpAdd
    | OpSub
    | OpLess
    | OpLessEq
    | OpMoreEq
    | OpMin
    | OpMax
    | OpMore
    | OpMod
    deriving (Show, Eq)

data UnOp = OpNeg
  | OpDeg
  | OpRad
  | OpAbs
  | OpSin
  | OpCos
  | OpFloor
  deriving (Show, Eq)

-- | AST representing a simple expression
data SimpleExpression
    = Ident Ident
    | NumLit ArmaNumber
    | BinOp BinOp SimpleExpression SimpleExpression
    | UnOp UnOp SimpleExpression
    | Factor ArmaNumber ArmaNumber SimpleExpression
    | Interpolate ArmaNumber ArmaNumber ArmaNumber ArmaNumber SimpleExpression
    deriving (Show, Eq)