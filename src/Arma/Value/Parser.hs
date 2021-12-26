
{-|
Module      : Arma.Value.Parser
Description : Parses arma values from arma's `str` function
-}
{-# LANGUAGE TypeFamilies #-}

module Arma.Value.Parser(parseArmaValue) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer
import Data.Functor

import Data.Char
import Data.Void

import Arma.Value

tok :: Parsec Void String a -> Parsec Void String a
tok = lexeme (void $ takeWhileP (Just "space") isSpace)

-- | Parses an arma value from input string
parseArmaValue :: Parsec Void String ArmaValue
parseArmaValue = tok $ parseArmaNumber <|> parseArmaString <|> parseArmaArray

parseArmaNumber :: Parsec Void String ArmaValue
parseArmaNumber = tok $ ArmaNumber <$> signed (return ()) (try float <|> decimal)

parseArmaString :: Parsec Void String ArmaValue
parseArmaString = tok $ ArmaString <$> stringParser
    where
        stringParser = between stringTok stringTok stringContents
        stringTok = char '"'
        stringContents = takeWhileP (Just "string contents") (/= '"')

parseArmaArray :: Parsec Void String ArmaValue
parseArmaArray = ArmaArray <$> arrayParser 
    where
        openArray = tok $ char '['
        closeArray = tok $ char ']'
        comma = tok $ char ','
        arrayParser = between openArray closeArray (parseArmaValue `sepBy` comma)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left (f x)

connectEithers :: Either a b -> (b -> Either c d) -> Either (Either a c) d
connectEithers x f = mapLeft Left x >>= (mapLeft Right . f)