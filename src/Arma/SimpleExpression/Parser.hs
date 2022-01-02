{-# LANGUAGE OverloadedStrings #-}
module Arma.SimpleExpression.Parser
  ( Error
  , parseSimpleExpression
  ) where

import           Arma.SimpleExpression
import           Arma.Value
import           Control.Monad.Combinators.Expr
import           Data.Char
import           Data.Functor
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Data.Void
import           Text.Megaparsec
import qualified Text.Megaparsec.Char          as C
import qualified Text.Megaparsec.Char.Lexer    as CL

-- | Errors that can occur when parsing simple expresison
type Error = ParseErrorBundle Text Void

{-| Parses a simple expression from a string to its AST form (needs to be evaluated)
Currently a placeholder, always evaluates to 0.
-}
parseSimpleExpression :: Text -> Either Error SimpleExpression
parseSimpleExpression = runParser (C.space *> exprP <* eof) ""

tok :: Parsec Void Text a -> Parsec Void Text a
tok = CL.lexeme C.space

symbol :: Text -> Parsec Void Text Text
symbol = CL.symbol C.space

exprP :: Parsec Void Text SimpleExpression
exprP = makeExprParser
  termP
  [ [Prefix (UnOp OpNeg <$ symbol "-")]
  , [Prefix (UnOp OpDeg <$ symbol "deg"), Prefix (UnOp OpRad <$ symbol "rad")]
  , [InfixL (BinOp OpMul <$ symbol "*"), InfixL (BinOp OpDiv <$ symbol "/")]
  , [InfixL (BinOp OpAdd <$ symbol "+"), InfixL (BinOp OpSub <$ symbol "-")]
  , [InfixN (BinOp OpMore <$ symbol "<"), InfixL (BinOp OpMore <$ symbol ">")]
  ]

-- | Parses an arma value from input string
termP :: Parsec Void Text SimpleExpression
termP =
  tok
    $ let brackets = between (symbol "(") (symbol ")") exprP
          number =
            NumLit <$> CL.signed (return ()) (try CL.float <|> CL.decimal)

          firstIdentChar x = isAlpha x || x == '_'
          restIdentChar x = isAlphaNum x || x == '_'

          ident =
            Ident
              .   T.toLower
              <$> (   T.cons
                  <$> satisfy firstIdentChar
                  <*> takeWhile1P (Just "ident") restIdentChar
                  )
      in  brackets <|> number <|> ident
