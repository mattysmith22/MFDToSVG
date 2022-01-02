
{-|
Module      : Arma.Value.Parser
Description : Parses arma values from arma's `str` function
-}
{-# LANGUAGE TypeFamilies #-}

module Arma.Value.Parser(parseArmaValue) where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as CL
import Data.Functor

import Data.Char
import Data.Void

import Arma.Value
import Data.Text (Text)

tok :: Parsec Void Text a -> Parsec Void Text a
tok = lexeme (void $ takeWhileP (Just "space") isSpace)
{-# INLINE tok #-}

-- | Parses an arma value from input string
parseArmaValue :: Parsec Void Text ArmaValue
parseArmaValue = tok $ parseArmaNumber <|> parseArmaString <|> parseArmaArray

parseArmaNumber :: Parsec Void Text ArmaValue
parseArmaNumber = tok $ ArmaNumber <$> signed (return ()) (try float <|> decimal)
{-# INLINE parseArmaNumber #-}

parseArmaString :: Parsec Void Text ArmaValue
parseArmaString = tok $ ArmaString <$> stringParser
    where
        stringParser = between stringTok stringTok stringContents
        stringTok = char '"'
        stringContents = takeWhileP (Just "string contents") (/= '"')
{-# INLINE parseArmaString #-}

parseArmaArray :: Parsec Void Text ArmaValue
parseArmaArray = ArmaArray <$> arrayParser 
    where
        openArray = tok $ char '['
        closeArray = tok $ char ']'
        comma = tok $ char ','
        arrayParser = between openArray closeArray (parseArmaValue `sepBy` comma)
{-# INLINE parseArmaArray #-}

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right x) = Right x
mapLeft f (Left x) = Left (f x)
{-# INLINE mapLeft #-}