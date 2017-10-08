module Main where

import Data.Char (toLower)
import Data.Text (pack, unpack, breakOn, Text, tail, strip)
import System.IO
import Control.Monad (forever)
import System.Random (randomRIO)

data Card = Card
  { front :: Text
  , back :: Text
  }

filePath :: String
filePath = "./test.csv"

main = do
    hSetBuffering stdin NoBuffering
    putStrLn $ "Reading in from file: " ++ filePath
    raw <- readFile filePath
    putStrLn "Press Ctrl-C / Ctrl-D to exit, depending on your system."
    putStrLn ""
    doUntilQ $ parseFileContents raw

doUntilQ :: [Card] -> IO ()
doUntilQ cards = forever $ do
  randCard <- pickRandOne cards
  interactiveShow randCard
  doUntilQ cards

pickRandOne :: [a] -> IO a
pickRandOne xs = fmap (xs !!) (randomRIO (0, length xs - 1))

interactiveShow :: Card -> IO ()
interactiveShow card = do
  putStrLn "Press any key when ready to see the answer and when ready to proceed..."
  putStrLn $ "Q: " ++ (unpack $ front card)
  x <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  putStrLn $ "A: " ++ (unpack $ back card)
  putStrLn ""
  x <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  return ()

parseFileContents :: String -> [Card]
parseFileContents s = map splitToCard $ lines s

splitToCard :: String -> Card
splitToCard s = Card {front = f, back = b}
  where cardTuple = breakOn (pack ",") $ pack s
        f = fst cardTuple
        b = Data.Text.strip $ Data.Text.tail $ snd cardTuple
