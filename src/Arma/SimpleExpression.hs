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
    | OpMore
    deriving (Show, Eq)

data UnOp = OpNeg
  | OpDeg
  | OpRad
  deriving (Show, Eq)

-- | AST representing a simple expression
data SimpleExpression
    = Ident Ident
    | NumLit ArmaNumber
    | BinOp BinOp SimpleExpression SimpleExpression
    | UnOp UnOp SimpleExpression
    deriving (Show, Eq)

data EvalError
    = MissingIdent Ident
    | DivZero
    deriving (Show, Eq)

performBinOp
  :: BinOp -> ArmaNumber -> ArmaNumber -> Either EvalError ArmaNumber
performBinOp OpMul  l r = Right $ l * r
performBinOp OpDiv  l r = if r == 0 then Left DivZero else Right $ l / r
performBinOp OpAdd  l r = Right $ l + r
performBinOp OpSub  l r = Right $ l - r
performBinOp OpLess l r = Right $ if l < r then 1 else 0
performBinOp OpMore l r = Right $ if l > r then 1 else 0

degToRad :: ArmaNumber -> ArmaNumber
degToRad x = x * pi / 180

radToDeg :: ArmaNumber -> ArmaNumber
radToDeg x = x / pi * 180

performUnOp :: UnOp -> ArmaNumber -> Either EvalError ArmaNumber
performUnOp OpNeg x = Right $ negate x
performUnOp OpDeg x = Right $ radToDeg x
performUnOp OpRad x = Right $ degToRad x

evalNoVariables :: SimpleExpression -> Either EvalError ArmaNumber
evalNoVariables = (`eval` const Nothing)

eval
  :: SimpleExpression
  -> (Ident -> Maybe ArmaNumber)
  -> Either EvalError ArmaNumber
eval (Ident ident) lookupFunc = case lookupFunc ident of
  Nothing  -> Left $ MissingIdent ident
  (Just x) -> Right x
eval (NumLit x    ) _          = return x
eval (BinOp op l r) lookupFunc = do
  l' <- eval l lookupFunc
  r' <- eval r lookupFunc
  performBinOp op l' r'
eval (UnOp op x) lookupFunc = do
  x' <- eval x lookupFunc
  performUnOp op x'

addRequiredIdents :: SimpleExpression -> Set.Set Ident -> Set.Set Ident
addRequiredIdents (Ident  ident) = Set.insert ident
addRequiredIdents (NumLit _    ) = id
addRequiredIdents (BinOp _ l r ) = addRequiredIdents l . addRequiredIdents r
addRequiredIdents (UnOp _ x    ) = addRequiredIdents x

getRequiredIdents :: SimpleExpression -> Set.Set Ident
getRequiredIdents = (`addRequiredIdents` Set.empty)
