module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

import Data.Text (pack)
import CSVFC (maybeExtract, Card(..), parseFileContents, maybeInsertAtK)

main :: IO ()
main = do
  runHspec

runHspec :: IO ()
runHspec = hspec $ do
  describe "maybeInsertAtK" $ do
    it "should be the only value in the list if given an empty list" $ do
      maybeInsertAtK 0 (Just 10) ([] :: [Int]) `shouldBe` [10]

    it "should be the only value in the list if given an empty list even if the index is wrong" $ do
      maybeInsertAtK (-1) (Just 10) ([] :: [Int]) `shouldBe` [10]

    it "should be the only value in the list if given an empty list even if the index is really wrong" $ do
      maybeInsertAtK 100 (Just 10) ([] :: [Int]) `shouldBe` [10]

    it "should insert the value at the start if a negative index is given" $ do
      maybeInsertAtK (-2) (Just 10) ([1, 2, 3] :: [Int]) `shouldBe` [10, 1, 2, 3]

    it "should insert the value at the start if zero is given for the index" $ do
      maybeInsertAtK 0 (Just 10) ([1, 2, 3] :: [Int]) `shouldBe` [10, 1, 2, 3]

    it "should insert the value at the end if the index is larger than the length of the list" $ do
      maybeInsertAtK 4 (Just 10) ([1, 2, 3] :: [Int]) `shouldBe` [1, 2, 3, 10]

    it "should insert the value at the end if the index is a lot larger than the length of the list" $ do
      maybeInsertAtK 42 (Just 10) ([1, 2, 3] :: [Int]) `shouldBe` [1, 2, 3, 10]

    it "should insert the value at the in the middle according to the index" $ do
      maybeInsertAtK 1 (Just 10) ([1, 2, 3] :: [Int]) `shouldBe` [1, 10, 2, 3]

    it "should not change the list if we are inserting nothing" $ do
      maybeInsertAtK 1 Nothing ([1, 2, 3] :: [Int]) `shouldBe` [1, 2, 3]

  describe "maybeExtract" $ do
    it "should return Nothing on an empty list of ints" $ do
      maybeExtract 0 ([] :: [Int]) `shouldBe` (Nothing, [])

    it "should return Just 1 on list [1] of ints" $ do
      maybeExtract 0 ([1] :: [Int]) `shouldBe` (Just 1, [])

    it "should convert (0, [1,2,3,4,5]) to (Just 1, [2,3,4,5])" $ do
      maybeExtract 0 ([1,2,3,4,5] :: [Int]) `shouldBe` (Just 1, [2,3,4,5])

    it "should convert (4, [1,2,3,4,5]) to (Just 5, [1,2,3,4])" $ do
      maybeExtract 4 ([1,2,3,4,5] :: [Int]) `shouldBe` (Just 5, [1,2,3,4])

    it "should convert (100, [1,2,3,4,5]) to (Just 5, [1,2,3,4])" $ do
      maybeExtract 100 ([1,2,3,4,5] :: [Int]) `shouldBe` (Just 5, [1,2,3,4])

    it "should convert (3, [1,2,3,4,5]) to (Just 4, [1,2,3,5])" $ do
      maybeExtract 3 ([1,2,3,4,5] :: [Int]) `shouldBe` (Just 4, [1,2,3,5])

    it "should return Nothing and the original list for a negative index" $ do
      maybeExtract (-3) ([1,2,3,4,5] :: [Int]) `shouldBe` (Nothing, [1,2,3,4,5])

    it "should be the same list if we sort [Int] or we remove on element, prepend it, and sort that" $ do
      property (prop_maybeExtractSortedIfHasOrd :: Int -> [Int] -> Bool)

    it "should be the same list if we sort [String] or we remove on element, prepend it, and sort that" $ do
      property (prop_maybeExtractSortedIfHasOrd :: Int -> [Int] -> Bool)

    it "should be the same list if we sort [Card] or we remove on element, prepend it, and sort that" $ do
      property (prop_maybeExtractSortedIfHasOrd :: Int -> [Card] -> Bool)

    it "should have one less in the output list of Strings for positive indexes" $ do
      property (prop_outputListHasOneLessThanOriginalForPositiveIntegers :: Int -> [String] -> Bool)

    it "should have one less in the output list of Cards for positive indexes" $ do
      property (prop_outputListHasOneLessThanOriginalForPositiveIntegers :: Int -> [Card] -> Bool)

  describe "parseFileContents" $ do
    it "should parse Nothing if given nothing" $ do
      parseFileContents ([] :: String) `shouldBe` ([] :: [Card])

    it "should parse Nothing if given a comment '#'" $ do
      parseFileContents "#" `shouldBe` ([] :: [Card])

    it "should parse Nothing if given a comment line '#' + other text" $ do
      parseFileContents "# bhadkfjd dkjfdkj kdjfd " `shouldBe` ([] :: [Card])

    it "should parse Nothing if given a multiple comment lines of '#'" $ do
      parseFileContents "#    \n#    \n#" `shouldBe` ([] :: [Card])

    it "should not parse a comment after a valid card" $ do
      let c = Card { front = (pack "front"), back = (pack "back") }
      parseFileContents "front, back # this is a comment and should be ignored" `shouldBe` [c]

    it "should parse one card" $ do
      length (parseFileContents "front, back") `shouldBe` 1

    it "should parse the front of the card before the comma and the back after" $ do
      let c = Card { front = (pack "front"), back = (pack "back") }
      parseFileContents "front, back" `shouldBe` [c]

    it "should still parse the card with multiple comment lines" $ do
      let pStr = "#hii\nfront, back\n# dkfjd\n"
      let c = Card { front = (pack "front"), back = (pack "back") }
      parseFileContents pStr `shouldBe` [c]

    it "should be able to have commas in the back of the card. AKA the first comma should delimit the card" $ do
      let pStr = "front, back,,,more text here, yeah"
      let c = Card { front = (pack "front"), back = (pack "back,,,more text here, yeah") }
      parseFileContents pStr `shouldBe` [c]

    it "should parse two cards with \\n as a newline" $ do
      let pStr = "front, back\nfront,back"
      let c = Card { front = (pack "front"), back = (pack "back") }
      parseFileContents pStr `shouldBe` [c, c]

    it "should parse two cards with \\r\\n as a CRLF" $ do
      let pStr = "front, back\r\nfront,back"
      let c = Card { front = (pack "front"), back = (pack "back") }
      parseFileContents pStr `shouldBe` [c, c]


instance Arbitrary Card where
  arbitrary = genCard

genCard :: Gen Card
genCard = do
  f <- arbitrary
  b <- arbitrary
  return Card { front = (pack f), back = (pack b) }

prop_maybeExtractSortedIfHasOrd :: Ord a => Int -> [a] -> Bool
prop_maybeExtractSortedIfHasOrd i as = (sort as) == (sort as')
  where as' = a' ++ as_rem
        (maybe_a, as_rem) = maybeExtract i as
        a' = case maybe_a of
                  Nothing -> []
                  Just ja -> [ja]

prop_outputListHasOneLessThanOriginalForPositiveIntegers :: Int -> [a] -> Bool
prop_outputListHasOneLessThanOriginalForPositiveIntegers index original_list
  | index < 0 = True
  | otherwise = length original_list == newListLength
  where (_, newList) = maybeExtract index original_list
        newListLength = if length original_list == 0 then 0 else length newList + 1

