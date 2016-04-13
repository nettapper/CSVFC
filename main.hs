import Data.Text (pack, unpack, splitOn)
import System.IO

main :: IO ()
main = do
    raw <- readFile "/Users/Conner/Desktop/282Questions.txt"
    hSetBuffering stdin NoBuffering
    doUntilQ $ parse raw

doUntilQ :: [[String]] -> IO ()
doUntilQ raw = do
    x <- getLine
    case x of
         "q" -> return ()
         "Q" -> return ()
         _   ->  do
             print "The first question is"
             --print $ parse raw
             printCard raw
             doUntilQ raw

printCard :: [[String]] -> IO ()
printCard raw = do
    print $ head $ head raw
    x <- getLine
    print $ head $ tail $ head raw


parse :: String -> [[String]]
parse s = map splitToTuple $ lines s

splitToTuple :: String -> [String]
splitToTuple s = map unpack $ splitOn (pack ",") $ pack s
