import Data.Text (pack, unpack, splitOn)
import System.IO

main :: IO ()
main = do
    raw <- readFile "/Users/Conner/Desktop/282Questions.txt"
    hSetBuffering stdin NoBuffering
    doUntilQ raw

doUntilQ :: String -> IO ()
doUntilQ raw = do
    x <- getLine
    case x of
         "q" -> return ()
         "Q" -> return ()
         _   ->  do
             print ("The first question is" : [x])
             print $ parse raw
             doUntilQ raw

parse :: String -> [[String]]
parse s = map splitToTuple $ lines s

splitToTuple :: String -> [String]
splitToTuple s = map unpack $ splitOn (pack ",") $ pack s
