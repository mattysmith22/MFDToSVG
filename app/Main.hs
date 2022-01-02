module Main where

import Arma.Value.Parser
import Text.Megaparsec
import Text.Pretty.Simple
import Data.Void
import System.Exit
import Arma.Config.Parser
import Arma.MFD.Parser (MFDParserError, parseMfd)
import Arma.Config (Config, fromArmaValue)
import Arma.Value

pPrintOptions =OutputOptions {
    outputOptionsIndentAmount = 3,
    outputOptionsPageWidth = 120,
    outputOptionsCompact = True,
    outputOptionsCompactParens = True,
    outputOptionsInitialIndent = 0,
    outputOptionsColorOptions = Just defaultColorOptionsDarkBg,
    outputOptionsStringStyle = EscapeNonPrintable
};

parseIO :: Parsec Void String a -> String -> IO a
parseIO parser input = case result of
        (Left err) -> do
            putStrLn (errorBundlePretty err)
            exitWith (ExitFailure 1)
        (Right x) -> return x
    where
        result = parse parser "" input

parseConfigIO :: ConfigParser MFDParserError a  -> Config -> IO a
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
    raw <- readFile "testMFD.txt"
    armaVal <- parseIO parseArmaValue raw
    config <- readConfigIO armaVal
    mfdConfig <- parseConfigIO parseMfd config
    pPrintOpt CheckColorTty  pPrintOptions mfdConfig