module Main(main) where

import qualified Data.Aeson as Aeson
import Arma.MFD.Sources.Depends
import qualified Data.Text.IO                  as TIO
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Text (Text)
import Text.Megaparsec
import Data.Void (Void)
import Arma.Config.Parser (ConfigParser, runConfigParser)
import Arma.MFD.Parser (MFDParserError, parseMfd, parsePylonMfd)
import Arma.Value (ArmaValue)
import System.Exit
import Arma.Config (fromArmaValue, Config)
import Arma.Value.Parser (parseArmaValue)
import Arma.MFD.Process (process, ProcessingContext(..))
import System.Directory
import Control.Monad
import System.FilePath
import System.Environment
import qualified Data.ByteString.Lazy.Char8 as BS

mLog :: Text -> IO ()
mLog x = TIO.putStrLn $ "[MFDtoSVG] " <> x

parseIO :: Parsec Void Text a -> Text -> IO a
parseIO parser input = case result of
  (Left err) -> do
    putStrLn (errorBundlePretty err)
    exitWith (ExitFailure 1)
  (Right x) -> return x
  where result = parse parser "" input

parseConfigIO :: ConfigParser MFDParserError a -> Arma.Config.Config -> IO a
parseConfigIO parser config = case runConfigParser parser config of
  (Left err) -> do
    print err
    exitWith (ExitFailure 1)
  (Right x) -> return x

readConfigIO :: ArmaValue -> IO Arma.Config.Config
readConfigIO val = case fromArmaValue val of
  (Left err) -> do
    print err
    exitWith (ExitFailure 1)
  (Right x) -> return x

getProcessCtx :: FilePath -> IO ProcessingContext
getProcessCtx pylonsDir = do
    pylonTypes <- listDirectory pylonsDir >>= filterM (doesDirectoryExist . (pylonsDir </>))
    mLog $ T.pack $ show pylonTypes
    parsedTypes <- forM pylonTypes $ \pylonType -> do
        let typeDir = pylonsDir </> pylonType
        magazines <- listDirectory typeDir
        mLog $ T.pack $ show magazines
        parsedMagazines <- forM magazines $ \magFile -> do
            let magPath = typeDir </> magFile
            mLog $ "Reading pylon config " <> T.pack magPath
            config <- TIO.readFile magPath >>= parseIO parseArmaValue >>= readConfigIO >>= parseConfigIO parsePylonMfd
            return (T.pack $ dropExtensions magFile, config)
        return (T.pack pylonType, Map.fromList parsedMagazines)
    let pylons = Map.fromList parsedTypes

    return $ ProcessingContext pylons


main :: IO ()
main = do
    mLog "Reading MFD Config from file"
    raw   <- TIO.readFile "reference/mpd.txt"
    armaVal   <- parseIO parseArmaValue raw
    config    <- readConfigIO armaVal
    mfdConfig <- parseConfigIO parseMfd config

    mLog "Getting processing context"
    processCtx <- getProcessCtx "reference/pylons"

    
    args <- getArgs
    case args of
      ("Deps":_) -> do
        let deps = getSourceDependencies $ process processCtx mfdConfig
        BS.putStrLn $ Aeson.encode $ defaultValues deps
      ("Run":_) -> return ()
      (x:_) -> putStrLn $ "Invalid value: '" <> x <> "'"
      [] -> putStrLn "Enter Deps or Run"