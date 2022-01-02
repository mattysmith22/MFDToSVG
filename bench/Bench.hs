import Criterion.Main
import Arma.Value.Parser
import Text.Megaparsec
import qualified Data.Text.IO as TIO

main = do
    text <- TIO.readFile "bench/testMFD.txt"
    seq text $ defaultMain [
        bench "Parsing ArmaValue" $ whnf (runParser parseArmaValue "") text]