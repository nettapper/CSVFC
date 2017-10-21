module Main where

import System.IO
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Data.Text (unpack)
import CSVFCore (Card, front, back, getNextCard, parseFileContents)

filePath :: String
filePath = "./test.csv"

main :: IO ()
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

interactiveShowCard :: Card -> IO ()
interactiveShowCard card = do
  putStrLn "Press any key when ready to see the answer and when ready to proceed..."
  putStrLn $ "Q: " ++ (unpack $ front card)
  _ <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  putStrLn $ "A: " ++ (unpack $ back card)
  putStrLn ""
  _ <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  return ()
