import Criterion.Main
import Arma.Value.Parser
import Text.Megaparsec

main = do
    text <- readFile "bench/testMFD.txt"
    seq text $ defaultMain [
        bench "Parsing ArmaValue" $(whnf (runParser parseArmaValue "") text)]