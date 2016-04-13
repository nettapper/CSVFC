import Data.Text (pack, unpack, splitOn)
import System.IO
import System.Random (randomRIO)

main :: IO ()
main = do
    raw <- readFile "/Users/Conner/Desktop/282Questions.txt"
    hSetBuffering stdin NoBuffering
    doUntilQ $ parse raw

doUntilQ :: [[String]] -> IO ()
doUntilQ cards = do
    x <- getChar
    case x of
         'q' -> return ()
         'Q' -> return ()
         _   ->  do
             print ""
             print "The question is..."
             printRandomCard cards
             doUntilQ cards

printRandomCard :: [[String]] -> IO ()
printRandomCard cards = do
    rcard <- pick cards
    printCard rcard

pick :: [a] -> IO a
pick xs = fmap (xs !!) (randomRIO (0, length xs - 1))

printCard :: [String] -> IO ()
printCard card = do
    print $  head card
    x <- getChar
    print $ head $ tail card


parse :: String -> [[String]]
parse s = map splitToTuple $ lines s

splitToTuple :: String -> [String]
splitToTuple s = map unpack $ splitOn (pack ",") $ pack s
