
{-|
Module      : Arma.MFD.Parser.Element
Description : Parses MFD elements from config
-}
module Arma.MFD.Parser.Element where
import Arma.Config.Parser
import Arma.MFD.Parser.Source
import Arma.MFD.Parser.Types
import Arma.MFD.Parser.Source
import Arma.MFD
import Arma.Value
import Control.Applicative
import Data.List.Extra
import Data.Maybe
import Control.Monad

-- |Parses a single element at the parser's position
parseElement :: Parser MFDElement
parseElement = do
    elementType <- wDefault "group" $ readString "type"
    case elementType of
        "group" -> parseGroup
        "line" -> parseLine
        "polygon" -> parsePolygon
        "text" -> parseText
        --"scale" -> parseScale TODO: Implement scale
        invalidType -> return $ MFDElementGroup [] Nothing Nothing Nothing Nothing-- userConfigError (InvalidType invalidType) ["type"] 

parseGroup :: Parser MFDElement 
parseGroup = do
    color <- optional $ parseColor "color"
    alpha <- optional $ parseSimpleExpression "alpha"
    clipTL <- optional $ readVec2 "clipTL"
    clipBR <- optional $ readVec2 "clipBR"
    condition <- optional $ parseSimpleExpression "condition"
    children <- onSubConfigs parseElement
    return $ MFDElementGroup {
        mfdElementChildren = children,
        mfdElementColor = color,
        mfdElementAlpha = alpha,
        mfdElementClip = (,) <$> clipTL <*> clipBR,
        mfdElementCondition = condition
    }

readLineType :: String -> Parser LineType
readLineType ident = do
    number <- wDefault 0 $ readNumber ident
    case number of
        0 -> return LineTypeFull
        1 -> return LineTypeDotted
        2 -> return LineTypeDashed
        3 -> return LineTypeDotDashed
        x -> userConfigError (InvalidLineType x) [ident]

toPointTransform ::  ConfigPath -> [ArmaValue] ->ConfigParser MFDParserError [MFDPointTransform]
toPointTransform path pointComponents = do
    res <- foldM f ([],Nothing,Nothing) (zip pointComponents [0..])
    case res of
        (points,Nothing,Nothing) -> return points
        (points,_,_) -> userConfigError InvalidPoint  (path ++ [show (length pointComponents - 1)])
    where
        
        f (points,bone,Nothing) (ArmaArray [x,y],i) = do
            x' <- castNumber (path ++ [show i, "0"]) x
            y' <- castNumber (path ++ [show i, "1"]) y
            return (points, bone, Just (x',y'))
        
        f (_,_,Just _) (ArmaArray [_,_],i)= userConfigError InvalidPoint (path ++ [show i])
        f (_,_,_) (ArmaArray _,i) = userConfigError InvalidPoint (path ++ [show i])

        f (points,Nothing,offset) (ArmaString bone,_) = return (points,Just bone, offset)
        f (points,Just _,offset) (ArmaString _,i) = userConfigError InvalidPoint (path ++ [show i])
        
        f (points,Nothing,Nothing) (ArmaNumber _,i) = userConfigError InvalidPoint (path ++ [show i])
        f (points,mBone,mOffset) (ArmaNumber weight,_) = return (points ++ [MFDPointTransform mBone (fromMaybe (0,0) mOffset) weight], Nothing, Nothing)

parseLine :: Parser MFDElement
parseLine = do
    width <- readNumber "width"
    lineType <- readLineType "lineType"
    rawPointsArray <- readArray "points"
    let splitPoints = split (\x -> fst x == ArmaArray []) (zip rawPointsArray [0..])
    points <- mapM (mapM readPoint) splitPoints
    return $ MFDElementLine {
        mfdElementPoints = points,
        mfdElementWidth = width,
        mfdElementLineType = lineType
    }
    where
        readPoint (ArmaArray pointComponents, index) = toPointTransform ["points", show index] pointComponents 
        readPoint (_, index) = userConfigError InvalidPoint ["points", show index]

parsePolygon :: Parser MFDElement
parsePolygon = do
    rawPoints <- readArray "points"
    points <- mapM readPoly (zip rawPoints [0..])
    return $ MFDElementPolygon points
    where
        readPoly (ArmaArray points,i) = mapM (readPolyPoint i) (zip points [0..])
        readPoly (_,i) = userConfigError InvalidPoint ["points", show i]
        
        readPolyPoint polyInd (ArmaArray pointTransforms, pointInd) = toPointTransform ["points", show polyInd, show pointInd] pointTransforms
        readPolyPoint polyInd (_,pointInd) = userConfigError InvalidPoint ["points", show polyInd, show pointInd]

readTextAlign :: Parser TextAlign
readTextAlign = do
    align <- readString "align"
    case align of
        "left" -> return TextAlignLeft 
        "right" -> return TextAlignRight 
        "center" -> return TextAlignCenter
        t -> userConfigError (InvalidTextAlign t) ["align"]

parseText :: Parser MFDElement
parseText = do
    textAlign <- readTextAlign
    scale <- readNumber "scale"
    source <- readStringSource
    sourceScale <- readNumber "sourceScale"
    sourceLength <- optional $ readNumber "sourceLength"
    sourcePrecision <- optional $ readNumber "sourcePrecision"
    pos <- readArray "pos" >>= toPointTransform ["pos"]
    right <- readArray "right" >>= toPointTransform ["right"]
    down <- readArray "down" >>= toPointTransform ["down"]
    return $ MFDElementText {
        mfdElementAlign = textAlign,
        mfdElementScale = scale,
        mfdElementSource = source,
        mfdElementSourceScale = sourceScale,
        mfdElementSourceLength = sourceLength,
        mfdElementSourcePrecision = sourcePrecision,
        mfdElementTextPos = pos,
        mfdElementTextRight = right,
        mfdElementTextDown = down
    }