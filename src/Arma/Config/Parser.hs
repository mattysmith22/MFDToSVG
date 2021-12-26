module Arma.Config.Parser where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Applicative
import Arma.SimpleExpression
import Arma.Config
import Arma.Value
import Data.Maybe (fromMaybe)


type ConfigPath = [String]

data ParserState = ParserState {
    stateConfig::Config,
    statePath::ConfigPath
}
    deriving (Show, Eq)

initParserState :: Config -> ParserState
initParserState config = ParserState config []

runConfigParser' :: ConfigParser u a -> ParserState -> Either (ParserError u) a
runConfigParser' x s = runExcept $ runReaderT (unConfigParser x) s

runConfigParser :: ConfigParser u a -> Config -> Either (ParserError u) a
runConfigParser x s = runConfigParser' x $ initParserState s

data ParserError u = MissingAttribute ConfigPath
    | IncorrectAttributeType ArmaType ArmaType 
    | MissingSubConfig ConfigPath
    | UserParserError ConfigPath u
    | SimpleExpressionError SimpleExpressionError 
    | InvalidVec2 ConfigPath
    | UnknownError
    deriving (Show, Eq)

data Tree a = Node [Tree a]
    | Leaf a

newtype ConfigParser u a = ConfigParser {
    unConfigParser :: ReaderT ParserState (Except (ParserError u)) a
}

instance Functor (ConfigParser u) where
    fmap f pa = ConfigParser $ fmap f (unConfigParser pa)

instance Applicative (ConfigParser u) where
    pf <*> pa = ConfigParser (unConfigParser pf <*> unConfigParser pa)

    pure x = ConfigParser (pure x)

instance Monad (ConfigParser u) where
    pa >>= f = ConfigParser ( unConfigParser pa >>= unConfigParser . f)

instance Alternative (ConfigParser u) where
    pa <|> pb = ConfigParser $ do
        value <- ask
        case runConfigParser' pa value of
            (Left err) -> unConfigParser pb
            (Right x) -> return x

    empty = ConfigParser $ lift $ except (Left UnknownError)

wDefault :: Alternative m => a -> m a -> m a
wDefault def x = fromMaybe def <$> optional x

mustExist' :: ParserError u ->  Maybe a -> ReaderT ParserState (Except (ParserError u)) a
mustExist' err Nothing = lift $ except $ Left err
mustExist' _ (Just x) = return x

readAttribute :: String -> ConfigParser u ArmaValue 
readAttribute ident = ConfigParser $ do
    state <- ask
    mustExist' (MissingAttribute $ statePath state ++ [ident])
        $ lookup ident (properties $ stateConfig state)

readArray :: String -> ConfigParser u [ArmaValue]
readArray ident = ConfigParser $ do
    rawValue <- unConfigParser (readAttribute ident)
    case rawValue of
        (ArmaArray value) -> return value
        _ -> lift $ except $ Left $ IncorrectAttributeType ArmaTypeArray (getArmaType rawValue)

readString :: String -> ConfigParser u String
readString ident = ConfigParser $ do
    rawValue <- unConfigParser (readAttribute ident)
    case rawValue of
        (ArmaString value) -> return value
        _ -> lift $ except $ Left $ IncorrectAttributeType ArmaTypeString (getArmaType rawValue)

readSimpleExpression' :: String -> ConfigParser u SimpleExpression 
readSimpleExpression' str = case parseSimpleExpression str of
        Left err -> ConfigParser $ lift $ except $ Left $ SimpleExpressionError err
        Right x -> return x

readSimpleExpression :: String -> ConfigParser u SimpleExpression
readSimpleExpression ident = do
    str <- readString ident
    readSimpleExpression' str

readNumber :: String -> ConfigParser u ArmaNumber
readNumber ident = ConfigParser $ do
    rawValue <- unConfigParser (readAttribute ident)
    case rawValue of
        (ArmaNumber value) -> return value
        (ArmaString _) -> return 1 --Todo: Add numeric evaluation
        _ -> lift $ except $ Left $ IncorrectAttributeType ArmaTypeNumber (getArmaType rawValue)

readSimpleExpressionArray :: String -> ConfigParser u (Tree SimpleExpression)
readSimpleExpressionArray ident = ConfigParser $ do
    arr <- unConfigParser (readArray ident)
    unConfigParser $ Node <$> mapM readSimpleExpressionArray' arr
    where
        readSimpleExpressionArray' (ArmaNumber num) = return $ Leaf (NumLit num)
        readSimpleExpressionArray' (ArmaString str) = Leaf <$> readSimpleExpression' str
        readSimpleExpressionArray' (ArmaArray arr) = Node <$> mapM readSimpleExpressionArray' arr

readSubConfig :: String -> ConfigParser u Config
readSubConfig ident = ConfigParser $ do
    state <- ask
    mustExist' (MissingSubConfig $ statePath state ++ [ident])
        $ lookup ident (subClasses $ stateConfig state)

onSubConfig :: String -> ConfigParser u a -> ConfigParser u a
onSubConfig path parser = ConfigParser $ do
    state <- ask 
    subConfig <- unConfigParser (readSubConfig path)
    let newState = ParserState subConfig (statePath state ++ [path])
    local (const newState) $ unConfigParser parser

onSubConfigs :: ConfigParser u a -> ConfigParser u [a]
onSubConfigs parser = ConfigParser $ do
    state <- ask
    let runSubState (ident, cfg) = local (const $ ParserState cfg (statePath state ++ [ident])) $ unConfigParser parser
    mapM runSubState $ subClasses $ stateConfig state

onSubConfigs' :: (String -> ConfigParser u a) -> ConfigParser u [a]
onSubConfigs' parser = ConfigParser $ do
    state <- ask
    let runSubState (ident, cfg) = local (const $ ParserState cfg (statePath state ++ [ident])) $ unConfigParser (parser ident)
    mapM runSubState $ subClasses $ stateConfig state

userConfigError :: u -> ConfigPath -> ConfigParser u a
userConfigError err relPath = ConfigParser $ do
    path <- statePath <$> ask 
    lift $ except $ Left $ UserParserError (path ++ relPath) err

readVec2 :: String -> ConfigParser u (ArmaNumber, ArmaNumber)
readVec2 ident = do
    arr <- readArray ident
    case arr of
        [ArmaNumber x, ArmaNumber y] -> return (x,y)
        [_,_] -> return (1,1)--Todo: Add numeric evaluation
        _ ->  ConfigParser $ do
            path <- statePath <$> ask 
            lift $ throwE $ InvalidVec2 (path ++ [ident])