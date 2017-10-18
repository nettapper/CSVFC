module CSVFC
  ( Card
  , front
  , back
  , getNextCard
  , parseFileContents
  ) where

import Data.Text (pack, breakOn, Text, tail, strip)

data Card = Card
  { front :: Text
  , back :: Text
  } deriving (Show)

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

parseFileContents :: String -> [Card]
parseFileContents s = map splitToCard $ filter (not . aComment) $ lines s
  where aComment (s:_) = s == '#'

splitToCard :: String -> Card
splitToCard s = Card {front = f, back = b}
  where cardTuple = breakOn (pack ",") $ pack s
        f = fst cardTuple
        b = Data.Text.strip $ Data.Text.tail $ snd cardTuple
