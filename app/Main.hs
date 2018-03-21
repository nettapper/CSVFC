module Main where

import System.IO
import System.Random (randomRIO)
import System.Exit (exitSuccess)
import Data.Text (unpack)
import CSVFC (Card, front, back, getNextCard, parseFileContents, maybeInsertAtK)

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
         diff <- interactiveShowCard c
         case diff of
              'a' -> do
                idx <- randomRIO (0, length cs)
                let cs' = maybeInsertAtK idx mc cs
                doUntilQ cs'
              _ -> doUntilQ cs
       Nothing -> return ()
  return ()

interactiveShowCard :: Card -> IO Char
interactiveShowCard card = do
  putStrLn "Press any key when ready to see the answer and when ready to proceed..."
  putStrLn $ "\t Q: " ++ (unpack $ front card)
  _ <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  putStrLn $ "\t A: " ++ (unpack $ back card)
  diff <- wasCardDifficult
  return diff

wasCardDifficult :: IO Char
wasCardDifficult = do
  putStrLn "(a) to see this card again later. Press any other key to continue."
  putStrLn ""
  difficultly <- getChar
  putStr "\b"  -- a sneaky way to delete the getChar from above
  return difficultly
