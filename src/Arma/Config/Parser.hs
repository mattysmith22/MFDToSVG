{-|
Module      : Arma.Config.Parser
Description : Type for parsing information from config
-}
module Arma.Config.Parser
  ( ConfigPath
  , runConfigParser
  , ParserError(..)
  , Tree(..)
  , ConfigParser
  , wDefault
  , readAttribute
  , readArray
  , readString
  , readSimpleExpression
  , readSimpleExpressionArray
  , readNumber
  , readSubConfig
  , onSubConfig
  , onSubConfigs
  , onSubConfigs'
  , userConfigError
  , parseSimpleExpression
  , readVec2
  , castNumber
  ) where

import           Arma.Config
import qualified Arma.SimpleExpression         as SE
import           Arma.SimpleExpression.Eval
import qualified Arma.SimpleExpression.Parser  as SEP
import           Arma.Value
import           Control.Applicative
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Megaparsec                ( errorBundlePretty )
import           Vec

-- | Path to a config position, for example ["cfgVehicles", "fza_ah64d_b2e", "MFD"]
type ConfigPath = [Text]

data ParserState = ParserState
  { stateConfig :: Config
  , statePath   :: ConfigPath
  }
  deriving (Show, Eq)

initParserState :: Config -> ParserState
initParserState config = ParserState config []

runConfigParser' :: ConfigParser u a -> ParserState -> Either (ParserError u) a
runConfigParser' x s = runExcept $ runReaderT (unConfigParser x) s

-- | Runs a defined config parser against a given input config
runConfigParser :: ConfigParser u a -> Config -> Either (ParserError u) a
runConfigParser x s = runConfigParser' x $ initParserState s

-- | Errors that may occur while parsing
data ParserError u = MissingAttribute ConfigPath
    | IncorrectAttributeType ConfigPath ArmaType ArmaType
    | MissingSubConfig ConfigPath
    | UserParserError ConfigPath u
    | SimpleExpressionParseError SEP.Error
    | InvalidVec2 ConfigPath
    | UnknownError
    deriving (Show, Eq)

-- | Tree structure of any input type. Used when parsing multi-level arrays
data Tree a = Node [Tree a]
    | Leaf a

-- | Base composable config parsing type
newtype ConfigParser u a = ConfigParser {
    unConfigParser :: ReaderT ParserState (Except (ParserError u)) a
}

instance Functor (ConfigParser u) where
  fmap f pa = ConfigParser $ fmap f (unConfigParser pa)

instance Applicative (ConfigParser u) where
  pf <*> pa = ConfigParser (unConfigParser pf <*> unConfigParser pa)

  pure x = ConfigParser (pure x)

instance Monad (ConfigParser u) where
  pa >>= f = ConfigParser (unConfigParser pa >>= unConfigParser . f)

instance Alternative (ConfigParser u) where
  pa <|> pb = ConfigParser $ do
    value <- ask
    case runConfigParser' pa value of
      (Left  err) -> unConfigParser pb
      (Right x  ) -> return x

  empty = ConfigParser $ lift $ except (Left UnknownError)

-- | Returns default value if a parser fails
wDefault
  :: Alternative m
  => a -- ^ Default value
  -> m a -- ^ Parser that may fail
  -> m a -- ^ Parser that will return default value rather than fail.
wDefault def x = fromMaybe def <$> optional x

mustExist'
  :: ParserError u -> Maybe a -> ReaderT ParserState (Except (ParserError u)) a
mustExist' err Nothing  = lift $ except $ Left err
mustExist' _   (Just x) = return x

configPath :: ConfigParser u ConfigPath
configPath = ConfigParser (statePath <$> ask)

-- |Reads an Arma property at a given name, doesn't perform any type checks
readAttribute :: Text -> ConfigParser u ArmaValue
readAttribute ident = ConfigParser $ do
  state <- ask
  mustExist' (MissingAttribute $ statePath state ++ [ident])
    $ lookup ident (properties $ stateConfig state)

-- |Reads an Arma array from a property. Fails if the value at property is an incorrect type
readArray :: Text -> ConfigParser u [ArmaValue]
readArray ident = ConfigParser $ do
  rawValue <- unConfigParser (readAttribute ident)
  case rawValue of
    (ArmaArray value) -> return value
    _                 -> do
      path <- unConfigParser configPath
      lift $ except $ Left $ IncorrectAttributeType (path ++ [ident])
                                                    ArmaTypeArray
                                                    (getArmaType rawValue)

-- |Reads an Arma string from a property. Fails if the value at property is an incorrect type
readString :: Text -> ConfigParser u Text
readString ident = ConfigParser $ do
  rawValue <- unConfigParser (readAttribute ident)
  case rawValue of
    (ArmaNumber value) -> return $ T.pack $ show value
    (ArmaString value) -> return value
    _                  -> do
      path <- unConfigParser configPath
      lift $ except $ Left $ IncorrectAttributeType (path ++ [ident])
                                                    ArmaTypeString
                                                    (getArmaType rawValue)

-- |Reads an Arma simple expression from a string. Fails if the simple expression is invalid
readSimpleExpression :: Text -> ConfigParser u SE.SimpleExpression
readSimpleExpression str = case SEP.parseSimpleExpression str of
  Left err ->
    ConfigParser $ lift $ except $ Left $ SimpleExpressionParseError err
  Right x -> return x

evalSimpleExpressionConf :: SE.SimpleExpression -> ConfigParser u ArmaNumber
evalSimpleExpressionConf expr = return $ evalSimpleExpression expr mempty

readEvalSimpleExpression :: Text -> ConfigParser u ArmaNumber
readEvalSimpleExpression str =
  readSimpleExpression str >>= evalSimpleExpressionConf

-- |Reads an Arma simple expression from a property.
parseSimpleExpression :: Text -> ConfigParser u SE.SimpleExpression
parseSimpleExpression ident = do
  str <- readString ident
  readSimpleExpression str

-- |Reads an Arma string from a property. Fails if the value at property is an incorrect type
readNumber :: Text -> ConfigParser u ArmaNumber
readNumber ident = ConfigParser $ do
  rawValue <- unConfigParser (readAttribute ident)
  case rawValue of
    (ArmaNumber value) -> return value
    (ArmaString x) ->
      unConfigParser $ readSimpleExpression x >>= evalSimpleExpressionConf
    _ -> do
      path <- unConfigParser configPath
      lift $ except $ Left $ IncorrectAttributeType (path ++ [ident])
                                                    ArmaTypeNumber
                                                    (getArmaType rawValue)

-- |Reads an Arma simple expression from a property.
readSimpleExpressionArray :: Text -> ConfigParser u (Tree SE.SimpleExpression)
readSimpleExpressionArray ident = ConfigParser $ do
  arr <- unConfigParser (readArray ident)
  unConfigParser $ Node <$> mapM readSimpleExpressionArray' arr
 where
  readSimpleExpressionArray' (ArmaNumber num) = return $ Leaf (SE.NumLit num)
  readSimpleExpressionArray' (ArmaString str) =
    Leaf <$> readSimpleExpression str
  readSimpleExpressionArray' (ArmaArray arr) =
    Node <$> mapM readSimpleExpressionArray' arr

-- | Reads a sub-config's value
readSubConfig :: Text -> ConfigParser u Config
readSubConfig ident = ConfigParser $ do
  state <- ask
  mustExist' (MissingSubConfig $ statePath state ++ [ident])
    $ lookup ident (subClasses $ stateConfig state)

-- | Performs a given parser on a given sub-config path
onSubConfig :: Text -> ConfigParser u a -> ConfigParser u a
onSubConfig path parser = ConfigParser $ do
  state     <- ask
  subConfig <- unConfigParser (readSubConfig path)
  let newState = ParserState subConfig (statePath state ++ [path])
  local (const newState) $ unConfigParser parser

-- | Performs a given parser on all subconfigs, and collates the results.
onSubConfigs :: ConfigParser u a -> ConfigParser u [a]
onSubConfigs parser = onSubConfigs' (const parser)

-- | Performs a given parser on all subconfigs, and collates the results.
onSubConfigs' :: (Text -> ConfigParser u a) -> ConfigParser u [a]
onSubConfigs' parser = ConfigParser $ do
  state <- ask
  let runSubState (ident, cfg) =
        local (const $ ParserState cfg (statePath state ++ [ident]))
          $ unConfigParser (parser ident)
  mapM runSubState $ subClasses $ stateConfig state

{-| Throws a user error at the given path (relative to current parser position)

e.g.: if the parser was at ["cfgVehicles", "fza_ah64d_b2e"] and the path to
userConfigError was ["mfd"], the end path of the error would be 
["cfgVehicles", "fza_ah64d_b2e", "mfd"]
-}
userConfigError :: u -> ConfigPath -> ConfigParser u a
userConfigError err relPath = ConfigParser $ do
  path <- statePath <$> ask
  lift $ except $ Left $ UserParserError (path ++ relPath) err

-- | Reads a vector of two numbers from a config.
readVec2 :: Text -> ConfigParser u Vec2
readVec2 ident = do
  arr <- readArray ident
  case arr of
    [ArmaNumber x, ArmaNumber y] -> return $ V2 x y
    [ArmaString x, ArmaNumber y] -> do
      x' <- readEvalSimpleExpression x
      return $ V2 x' y
    [ArmaNumber x, ArmaString y] -> do
      y' <- readEvalSimpleExpression y
      return $ V2 x y'
    [ArmaString x, ArmaString y] -> do
      x' <- readEvalSimpleExpression x
      y' <- readEvalSimpleExpression y
      return $ V2 x' y'
    _ -> ConfigParser $ do
      path <- statePath <$> ask
      lift $ throwE $ InvalidVec2 (path ++ [ident])

castNumber :: ConfigPath -> ArmaValue -> ConfigParser u ArmaNumber
castNumber _    (ArmaNumber n) = return n
castNumber _    (ArmaString x) = readEvalSimpleExpression x
castNumber path val            = ConfigParser $ do
  lift $ except $ Left $ IncorrectAttributeType path
                                                ArmaTypeNumber
                                                (getArmaType val)
