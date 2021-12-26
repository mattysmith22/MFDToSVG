{-|
Module      : Arma.SimpleExpression
Description : Simple expression evaluation library
-}
module Arma.SimpleExpression where
import Arma.Value

-- | AST representing a simple expression
newtype SimpleExpression = NumLit ArmaNumber
    deriving (Show, Eq)

-- | Errors that can occur when parsing simple expresison
type SimpleExpressionError = ()

{-| Parses a simple expression from a string to its AST form (needs to be evaluated)
Currently a placeholder, always evaluates to 0.
-}
parseSimpleExpression :: String -> Either SimpleExpressionError SimpleExpression
parseSimpleExpression = const $ Right $ NumLit 0