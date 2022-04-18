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
import           Arma.MFD.Sources.Depends
import           Arma.MFD.Sources.Values
import           Arma.MFD.Draw
import           Data.Yaml

{-
pPrintOptions = OutputOptions
  { outputOptionsIndentAmount  = 3
  , outputOptionsPageWidth     = 120
  , outputOptionsCompact       = True
  , outputOptionsCompactParens = True
  , outputOptionsInitialIndent = 0
  , outputOptionsColorOptions  = Just defaultColorOptionsDarkBg
  , outputOptionsStringStyle   = EscapeNonPrintable
  }
-}
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
    print err
    exitWith (ExitFailure 1)
  (Right x) -> return x

readConfigIO :: ArmaValue -> IO Config
readConfigIO value = case fromArmaValue value of
  (Left err) -> do
    print err
    exitWith (ExitFailure 1)
  (Right x) -> return x

mLog :: Text -> IO ()
mLog x = TIO.putStrLn $ "[MFDtoSVG] " <> x

main :: IO ()
main = do
  mLog "Reading MFD Config from file"
  raw       <- TIO.readFile "bench/testMFD.txt"

  mLog "Parsing arma value from file"
  armaVal   <- parseIO parseArmaValue raw

  mLog "Parsing arma config from arma value"
  config    <- readConfigIO armaVal

  mLog "Parsing MFD config from arma config"
  mfdConfig <- parseConfigIO parseMfd config

  mLog "Calculating dependencies"
  let dependencies = getSourceDependencies $ drawMFD mfdConfig
  encodeFile "sourcesTemplate.yaml" (defaultValues dependencies)

  _ <- decodeFileThrow "sources.yaml" :: IO SourceValues
  return ()