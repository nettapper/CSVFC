import Data.Text (pack, unpack, splitOn)

main :: IO ()
main = do
    raw <- readFile "/Users/Conner/Desktop/282Questions.txt"
    print $ parse raw

parse :: String -> [[String]]
parse s = map splitToTuple $ lines s

splitToTuple :: String -> [String]
splitToTuple s = map unpack $ splitOn (pack ",") $ pack s
