module Arma.SimpleExpression.Parser (Error, parseSimpleExpression) where

import Arma.Value
import Data.Void
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as CL
import Data.Functor
import Arma.SimpleExpression
import Control.Monad.Combinators.Expr
import Data.Char

-- | Errors that can occur when parsing simple expresison
type Error = ParseErrorBundle String Void

{-| Parses a simple expression from a string to its AST form (needs to be evaluated)
Currently a placeholder, always evaluates to 0.
-}
parseSimpleExpression :: String -> Either Error SimpleExpression
parseSimpleExpression = runParser (C.space *> exprP <* eof) ""

tok :: Parsec Void String a -> Parsec Void String a
tok = CL.lexeme C.space

symbol :: String -> Parsec Void String String
symbol = CL.symbol C.space

exprP :: Parsec Void String SimpleExpression
exprP = makeExprParser termP
        [[Prefix (UnOp OpNeg <$ symbol "-")]
        ,[InfixL (BinOp OpMul <$ symbol "*"),InfixL (BinOp OpDiv <$ symbol "/")]
        ,[InfixL (BinOp OpAdd <$ symbol "+"),InfixL (BinOp OpSub <$ symbol "-")]]

-- | Parses an arma value from input string
termP :: Parsec Void String SimpleExpression 
termP = tok $ let
    brackets = between (symbol "(") (symbol ")") exprP
    number = NumLit <$> CL.signed (return ()) (try CL.float <|> CL.decimal)

    firstIdentChar x = isAlpha x || x == '_'
    restIdentChar x = isAlphaNum x || x == '_'

    ident = Ident <$> ((:) <$> satisfy firstIdentChar <*> takeWhile1P (Just "ident") restIdentChar)
    in
        brackets <|> number <|> ident