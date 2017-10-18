module Main where

import Data.Char (toLower)
import Data.Text (pack, unpack, breakOn, Text, tail, strip)
import System.IO
import System.Random (randomRIO)
import System.Exit (exitSuccess)

data Card = Card
  { front :: Text
  , back :: Text
  } deriving (Show)

filePath :: String
filePath = "./test.csv"

main = do
    hSetBuffering stdin NoBuffering
    putStrLn $ "Reading in from file: " ++ filePath
    raw <- readFile filePath
    putStrLn "Press Ctrl-C / Ctrl-D to exit, depending on your system."
    let parsedCards = parseFileContents raw
    putStrLn $ "Successfully loaded " ++ show (length parsedCards) ++ " card(s)."
    putStrLn ""
    doUntilQ parsedCards
    putStrLn "Looks like you've reached the end of the file."
    putStrLn "Thanks for usings CSVFC."
    exitSuccess

doUntilQ :: [Card] -> IO ()
doUntilQ [] = return ()
doUntilQ cards = do
  i <- randomRIO (0, length cards - 1)
  let (mc, cs) = getNextCard cards i
  case mc of
       Just c -> do
         interactiveShowCard c
         doUntilQ cs
       Nothing -> return ()
  return ()

getNextCard :: [Card] -> Int -> (Maybe Card, [Card])
getNextCard = flip maybeExtract

maybeExtract :: Int -> [a] -> (Maybe a, [a])
maybeExtract _ [] = (Nothing, [])
maybeExtract i as =
  let (fhalf, bhalf) = splitAt (i + 1) as
   in case fhalf of
           [] -> (Nothing, bhalf)
           _ -> (Just (last fhalf), allButLast fhalf ++ bhalf)
  where allButLast = reverse . Prelude.tail . reverse


interactiveShowCard :: Card -> IO ()
interactiveShowCard card = do
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
parseFileContents s = map splitToCard $ filter (not . aComment) $ lines s
  where aComment (s:_) = s == '#'

splitToCard :: String -> Card
splitToCard s = Card {front = f, back = b}
  where cardTuple = breakOn (pack ",") $ pack s
        f = fst cardTuple
        b = Data.Text.strip $ Data.Text.tail $ snd cardTuple
