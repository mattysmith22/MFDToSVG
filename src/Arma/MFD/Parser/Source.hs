
{-|
Module      : Arma.MFD.Parser.Source
Description : Parser for MFD sources
-}
module Arma.MFD.Parser.Source where

import           Arma.Config.Parser
import           Arma.MFD
import           Arma.MFD.Parser.Types
import           Data.Char
import qualified Data.Text                     as T
import           Data.Text                      ( Text )

-- | Reads a float source from the config
readFloatSource :: ConfigParser u FloatSource
readFloatSource = do
  sourceType <- readString "source"
  case T.toLower sourceType of
    "user" -> FloatSourceUser . truncate <$> readNumber "sourceIndex"
    typ    -> return $ FloatSource typ

-- | Reads a string source from the config
readStringSource :: ConfigParser u (Either Text StringSource)
readStringSource = do
  sourceType <- readString "source"
  case T.toLower sourceType of
    "usertext" -> do
      Right . StringSourceUser . truncate <$> readNumber "sourceIndex"
    "time"   -> Right . StringSourceTime <$> readString "text"
    "static" -> Left <$> readString "text"
    typ      -> return $ Right $ StringSource typ
