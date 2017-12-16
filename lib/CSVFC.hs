module CSVFC
  ( Card(..)
  , parseFileContents
  , getNextCard
  , maybeExtract
  , maybeInsertAtK
  ) where

import Data.Text (pack, breakOn, Text, tail, strip)

data Card = Card
  { front :: Text
  , back :: Text
  } deriving (Show, Ord, Eq)

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

maybeInsertAtK :: Int -> Maybe a -> [a] -> [a]
maybeInsertAtK _ Nothing xs = xs
maybeInsertAtK index (Just a) xs
  | index <= 0 = a : xs
  | index >= (length xs) = reverse $ a : (reverse xs)
  | otherwise = (take index xs) ++ [a] ++ (drop index xs)

parseFileContents :: String -> [Card]
parseFileContents s = map splitToCard $ filter (not . isEmpty) $ map process $ lines s
  where isEmpty = (== [])
        process [] = []
        process (c:cs) = if c == '#' then [] else c : process cs

splitToCard :: String -> Card
splitToCard s = Card {front = f, back = b}
  where cardTuple = breakOn (pack ",") $ pack s
        f = fst cardTuple
        b = Data.Text.strip $ Data.Text.tail $ snd cardTuple
