{-|
Module      : Arma.SimpleExpression
Description : Simple expression evaluation library
-}
module Arma.SimpleExpression where
import Text.Megaparsec
import Arma.Value
import qualified Data.Map as Map
import Data.Text(Text)

type Ident = Text

data BinOp
    = OpMul
    | OpDiv
    | OpAdd
    | OpSub
    | OpLess
    | OpMore
    deriving (Show, Eq)

data UnOp
    = OpNeg
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

performBinOp :: BinOp -> ArmaNumber -> ArmaNumber -> Either EvalError ArmaNumber
performBinOp OpMul l r = Right $ l * r
performBinOp OpDiv l r = if r == 0 then Left DivZero else Right $ l / r
performBinOp OpAdd l r = Right $ l + r
performBinOp OpSub l r = Right $ l - r
performBinOp OpLess l r = Right $ if l < r then 1 else 0
performBinOp OpMore l r = Right $ if l > r then 1 else 0

performUnOp :: UnOp -> ArmaNumber -> Either EvalError ArmaNumber
performUnOp OpNeg x = Right $ negate x

evalNoVariables :: SimpleExpression -> Either EvalError ArmaNumber
evalNoVariables = (`eval` const Nothing)

eval :: SimpleExpression -> (Ident -> Maybe ArmaNumber) -> Either EvalError ArmaNumber
eval (Ident ident) lookupFunc = case lookupFunc ident of
    Nothing -> Left $ MissingIdent ident
    (Just x) -> Right x
eval (NumLit x) _ = return x
eval (BinOp op l r) lookupFunc = do
    l' <- eval l lookupFunc
    r' <- eval r lookupFunc
    performBinOp op l' r'
eval (UnOp op x) lookupFunc = do
    x' <- eval x lookupFunc
    performUnOp op x'