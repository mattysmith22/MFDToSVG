module Arma.SimpleExpression where
import Arma.Value

newtype SimpleExpression = NumLit ArmaNumber
    deriving (Show, Eq)

type SimpleExpressionError = ()

parseSimpleExpression :: String -> Either SimpleExpressionError SimpleExpression
parseSimpleExpression = const $ Right $ NumLit 0