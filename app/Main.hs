{-# LANGUAGE RecursiveDo #-}
module Main(main) where
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Arma.MFD.Sources.Depends
import Arma.MFD.Sources.Values
import qualified Data.Text.IO                  as TIO
import Text.Read (readMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Text (Text)
import Arma.MFD.Sources.Key (SourceKey(toSourceKey))
import Text.Megaparsec
import Data.Void (Void)
import Arma.Config.Parser (ConfigParser, runConfigParser)
import Arma.MFD.Parser (MFDParserError, parseMfd)
import Arma.Value (ArmaValue)
import System.Exit
import Arma.Config (fromArmaValue, Config)
import Arma.Value.Parser (parseArmaValue)
import Arma.MFD.Process (process)
import Arma.MFD
import Graphics.Svg (renderText)
import Arma.MFD.Draw
import Arma.MFD.Sources.With (runWithSource)
import Linear (V2(V2))
import Data.Maybe (fromMaybe)
import Control.Monad (void, unless)

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

main :: IO ()
main = do
    mLog "Reading MFD Config from file"
    raw   <- TIO.readFile "bench/testMFD.txt"

    mLog "Parsing arma value from file"
    armaVal   <- parseIO parseArmaValue raw

    mLog "Parsing arma config from arma value"
    config    <- readConfigIO armaVal

    mLog "Parsing MFD config from arma config"
    mfdConfig <- parseConfigIO parseMfd config

    mLog "Starting server"
    startGUI defaultConfig (setup mfdConfig)

setup :: UnProcessed MFD -> Window -> UI ()
setup mfd w = mdo
    let deps = getSourceDependencies $ process mfd
    ticketingUrl <- loadFile "application/octet-stream" "/home/mbs/Downloads/ticketing/ticketing/TICKETING/Ticketing.ttf"

    let drawConf = DrawContext
            { mfdSize = V2 500 500
            , armaFontMappings = Map.fromList [("fza_ticketing", "Ticketing")]
            , newFontMappings = [("Ticketing", [(T.pack ticketingUrl, "truetype")])]
            }
    (elemt, initSrc, srcChangeEvt, hookup) <- setupDeps deps beh
    beh <- accumB initSrc srcChangeEvt
    _ <- getBody w #+ [UI.table #+ [UI.tr #+
            [UI.td #+ [UI.div # set UI.id_ "result"]
            , elemt]]]

    hookup
    let drawWithSource sources = do
            let processedMFD = runWithSource (process mfd) sources
            let svg = TL.unpack $ renderText $ drawMFD drawConf processedMFD
            runFunction (setResult svg)
        
    drawWithSource initSrc
    _ <- onChanges beh drawWithSource

    return ()

setResult :: String -> JSFunction ()
setResult = ffi "document.getElementById(\"result\").innerHTML = %1"

promoteUp :: (a -> b) -> (a -> b -> a) -> Event (b -> b) -> Event (a -> a)
promoteUp getter setter = fmap (\f x -> setter x (f $ getter x))

setupDeps :: SourceDeps -> Behavior SourceValues -> UI (UI Element, SourceValues, Event (SourceValues -> SourceValues), UI ())
setupDeps deps@(floatDeps, stringDeps, boolDeps) unpBeh = do

        (floatElems, floatEvt, floatHookup) <- rowsForDict (Set.toList floatDeps) (T.unpack . toSourceKey) floatVal 0 (floatValues <$> unpBeh)
        (stringElems, stringEvt, stringHookup) <- rowsForDict (Set.toList stringDeps) (T.unpack . toSourceKey) stringVal "" (stringValues <$> unpBeh)
        (boolElems, boolEvt, boolHookup) <- rowsForDict (Set.toList boolDeps) (T.unpack . toSourceKey) boolVal False (boolValues <$> unpBeh)

        let floatEvt' = promoteUp floatValues (\x f -> x {floatValues = f}) floatEvt
        let stringEvt' = promoteUp stringValues (\x s -> x {stringValues = s}) stringEvt
        let boolEvt' = promoteUp boolValues (\x b -> x {boolValues = b}) boolEvt

        let hookup = floatHookup >> stringHookup >> boolHookup

        let aggEvt = concatenate <$> unions [floatEvt', stringEvt', boolEvt']

        let floatTable = UI.table #+
                ( UI.th #+ [UI.tr # set text "Float Source", UI.tr]
                : floatElems
                )
        let stringTable = UI.table #+
                ( UI.th #+ [UI.tr # set text "String Source", UI.tr]
                : stringElems
                )
        let boolTable = UI.table #+
                ( UI.th #+ [UI.tr # set text "Bool Source", UI.tr]
                : boolElems
                )

        let elemt = UI.table #+ [UI.tr #+
                [ UI.td #+ [floatTable]
                , UI.td #+ [stringTable]
                , UI.td #+ [boolTable]
                ]]
        return (elemt, defaultValues deps, aggEvt, hookup)
    where
        rowsForDict :: Ord a => [a] -> (a -> String) -> (Behavior b -> UI (Element, Event (b -> b), UI ())) -> b -> Behavior (Map a b) -> UI ([UI Element], Event (Map a b -> Map a b), UI ())
        rowsForDict keys showF editElem def beh = do
            let rowforDict key = do
                    (editor,evt, hookup) <- editElem (fromMaybe def . Map.lookup key <$> beh)
                    row' <- UI.tr #+ [UI.td #+ [string (showF key)], UI.td #+ [element editor]]
                    let evt' = (\x -> Map.update (Just . x) key) <$> evt
                    return (row', evt', hookup)
            (elems, evts, hookups) <- unzip3 <$> mapM rowforDict keys
            let evt = concatenate <$> unions evts
            return (fmap element elems, evt, sequence_ hookups)

        floatVal :: Behavior Double -> UI (Element, Event (Double -> Double), UI ())
        floatVal beh = do
            inp <- UI.input # set UI.type_ "text"

            editingBeh <- stepper False $ and <$> unions [True <$ UI.domEvent "focus" inp, False <$ UI.blur inp]

            let onceInit = onChanges beh $ \x -> do
                        editing <- currentValue editingBeh
                        unless editing $ void $ element inp # set UI.value (show x)
        
            on UI.blur inp $ \_ -> do
                val <- currentValue beh
                element inp # set UI.value (show val)

            let evt = fmap const $ filterJust $ readMaybe <$> UI.valueChange inp 

            return (inp, evt, onceInit)

        stringVal :: Behavior Text -> UI (Element, Event (Text -> Text), UI ())
        stringVal beh = do
            inp <- UI.input # set UI.type_ "text" # sink UI.value (T.unpack <$> beh)
            let evt = const . T.pack <$> UI.valueChange inp 
            return (inp, evt, return ())

        boolVal :: Behavior Bool -> UI (Element, Event (Bool -> Bool), UI ())
        boolVal beh = do
            inp <- UI.input # set UI.type_ "checkbox" # sink UI.checked beh
            let evt = const <$> UI.checkedChange inp
            return (inp, evt, return ())