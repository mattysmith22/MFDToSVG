module Main where

import           Arma.Config                    ( Config
                                                , fromArmaValue
                                                )
import           Arma.Config.Parser
import           Arma.MFD.Parser                ( MFDParserError
                                                , parseMfd
                                                )
import           Arma.Value
import           Arma.Value.Parser
import           Data.Text                      ( Text )
import qualified Data.Text.IO                  as TIO
import           Data.Void
import           System.Exit
import           Text.Megaparsec
import           Text.Pretty.Simple
import Arma.MFD.Sources (mfdGetSources)

pPrintOptions = OutputOptions
  { outputOptionsIndentAmount  = 3
  , outputOptionsPageWidth     = 120
  , outputOptionsCompact       = True
  , outputOptionsCompactParens = True
  , outputOptionsInitialIndent = 0
  , outputOptionsColorOptions  = Just defaultColorOptionsDarkBg
  , outputOptionsStringStyle   = EscapeNonPrintable
  }

parseIO :: Parsec Void Text a -> Text -> IO a
parseIO parser input = case result of
  (Left err) -> do
    putStrLn (errorBundlePretty err)
    exitWith (ExitFailure 1)
  (Right x) -> return x
  where result = parse parser "" input

parseConfigIO :: ConfigParser MFDParserError a -> Config -> IO a
parseConfigIO parser config = case runConfigParser parser config of
  (Left err) -> do
    pPrintOpt NoCheckColorTty pPrintOptions err
    exitWith (ExitFailure 1)
  (Right x) -> return x

readConfigIO :: ArmaValue -> IO Config
readConfigIO value = case fromArmaValue value of
  (Left err) -> do
    pPrintOpt NoCheckColorTty pPrintOptions err
    exitWith (ExitFailure 1)
  (Right x) -> return x

main :: IO ()
main = do
  raw       <- TIO.readFile "bench/testMFD.txt"
  armaVal   <- parseIO parseArmaValue raw
  config    <- readConfigIO armaVal
  mfdConfig <- parseConfigIO parseMfd config
  let sources = mfdGetSources mfdConfig
  pPrintOpt NoCheckColorTty pPrintOptions sources
