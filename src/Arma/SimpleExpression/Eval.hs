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
import Vec (clamp)

performBinOp :: BinOp -> ArmaNumber -> ArmaNumber -> ArmaNumber
performBinOp OpMul  l r = l * r
performBinOp OpDiv  l r = if r == 0 then 0 else l / r
performBinOp OpAdd  l r = l + r
performBinOp OpSub  l r = l - r
performBinOp OpLess l r = if l < r then 1 else 0
performBinOp OpLessEq l r = if l <= r then 1 else 0
performBinOp OpMore l r = if l > r then 1 else 0
performBinOp OpMoreEq l r = if l >= r then 1 else 0
performBinOp OpMin l r = l `min` r
performBinOp OpMax l r = l `max` r

degToRad :: ArmaNumber -> ArmaNumber
degToRad x = x * pi / 180

radToDeg :: ArmaNumber -> ArmaNumber
radToDeg x = x / pi * 180

performUnOp :: UnOp -> ArmaNumber -> ArmaNumber
performUnOp OpNeg = negate
performUnOp OpDeg = radToDeg
performUnOp OpRad = degToRad
performUnOp OpAbs = abs

-- r = x * (to - from) + from
-- r - from = x * (to - from)
-- (r - from) / (to - from) = x
evalSimpleExpression'
  :: SimpleExpression
  -> WithSource ArmaNumber
evalSimpleExpression' x= WithSource (eval' x)
    where
        eval' :: SimpleExpression -> (forall arr. WithSources arr => arr () ArmaNumber)
        eval' (Ident ident) = getFloat (readSimpleExpressionSource ident)
        eval' (NumLit x) = arr $ const x
        eval' (BinOp op l r) =
            arr (uncurry $ performBinOp op)
            . (eval' l &&& eval' r )
        eval' (UnOp op x) =
            arr (performUnOp op) . eval' x 
        eval' (Factor from to expr) =
            arr (clamp 0 1 . factor from to) . eval' expr
        eval' (Interpolate xFrom xTo resFrom resTo expr) =
            arr (\x -> factor xFrom xTo x * (resTo - resFrom) + resFrom) . eval' expr

        factor from to x = (x-from) / (to-from)

evalSimpleExpression
  :: SimpleExpression
  -> SourceValues
  -> ArmaNumber
evalSimpleExpression = runWithSource . evalSimpleExpression'

readSimpleExpressionSource :: Ident -> FloatSource
readSimpleExpressionSource (T.stripPrefix "user" -> Just num) =
  case TR.decimal num of
    (Left  _) -> FloatSource $ "user" <> num
    (Right (val, _)) -> FloatSourceUser val
readSimpleExpressionSource name = FloatSource name
