{-# LANGUAGE ViewPatterns, Rank2Types #-}
module Arma.SimpleExpression.Eval (evalSimpleExpression, evalSimpleExpression') where

import           Arma.SimpleExpression
import           Arma.Value
import           Arma.MFD
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Set as Set
import           Data.Text (Text)
import           Control.Arrow
import           Control.Category
import           Prelude hiding ((.))
import           Arma.MFD.Sources.With
import           Data.Bool (bool)
import Arma.MFD.Sources.Values (SourceValues)

boolSources :: Set.Set Text
boolSources = Set.fromList ["on"]

floatSources :: Set.Set Text
floatSources = Set.fromList ["altitudeAGL"]

performBinOp :: BinOp -> ArmaNumber -> ArmaNumber -> ArmaNumber
performBinOp OpMul  l r = l * r
performBinOp OpDiv  l r = if r == 0 then 0 else l / r
performBinOp OpAdd  l r = l + r
performBinOp OpSub  l r = l - r
performBinOp OpLess l r = if l < r then 1 else 0
performBinOp OpMore l r = if l > r then 1 else 0

degToRad :: ArmaNumber -> ArmaNumber
degToRad x = x * pi / 180

radToDeg :: ArmaNumber -> ArmaNumber
radToDeg x = x / pi * 180

performUnOp :: UnOp -> ArmaNumber -> ArmaNumber
performUnOp OpNeg x = negate x
performUnOp OpDeg x = radToDeg x
performUnOp OpRad x = degToRad x

evalSimpleExpression'
  :: SimpleExpression
  -> WithSource ArmaNumber
evalSimpleExpression' x= WithSource (eval' x)
    where
        eval' :: SimpleExpression -> (forall arr. WithSources arr => arr () ArmaNumber)
        eval' (Ident ident) = case readSimpleExpressionSource ident of
            Nothing -> arr $ const 0
            Just (Left boolSrc) -> arr (bool 1 0) . getBool boolSrc
            Just (Right floatSrc) -> getFloat floatSrc
        eval' (NumLit x) = arr $ const x
        eval' (BinOp op l r) =
            arr (uncurry $ performBinOp op)
            . (eval' l &&& eval' r )
        eval' (UnOp op x) =
            arr (performUnOp op) . eval' x 

evalSimpleExpression
  :: SimpleExpression
  -> SourceValues
  -> ArmaNumber
evalSimpleExpression = runWithSource . evalSimpleExpression'

readSimpleExpressionSource :: Ident -> Maybe (Either BoolSource FloatSource)
readSimpleExpressionSource (T.stripPrefix "user" -> Just num) =
  case TR.decimal num of
    (Left  err     ) -> Nothing
    (Right (val, _)) -> Just $ Right $ FloatSourceUser val
readSimpleExpressionSource name
  | name `Set.member` boolSources  = Just $ Left $ BoolSource name
  | name `Set.member` floatSources = Just $ Right $ FloatSource name
  | otherwise                      = Nothing
