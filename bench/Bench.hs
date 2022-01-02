import           Arma.Value.Parser
import           Criterion.Main
import qualified Data.Text.IO                  as TIO
import           Text.Megaparsec

main = do
  text <- TIO.readFile "bench/testMFD.txt"
  seq text $ defaultMain
    [bench "Parsing ArmaValue" $ whnf (runParser parseArmaValue "") text]
