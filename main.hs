import Data.Text (pack, unpack, splitOn)
import System.IO

main :: IO ()
main = do
    raw <- readFile "/Users/Conner/Desktop/282Questions.txt"
    hSetBuffering stdin NoBuffering
    doUntilQ $ parse raw

doUntilQ :: [[String]] -> IO ()
doUntilQ cards = do
    x <- getLine
    case x of
         "q" -> return ()
         "Q" -> return ()
         _   ->  do
             print "The first question is"
             printCard cards
             doUntilQ cards

printCard :: [[String]] -> IO ()
printCard cards = do
    print $ head $ head cards
    x <- getLine
    print $ head $ tail $ head cards


parse :: String -> [[String]]
parse s = map splitToTuple $ lines s

splitToTuple :: String -> [String]
splitToTuple s = map unpack $ splitOn (pack ",") $ pack s
