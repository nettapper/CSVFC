module Main where

import Test.Hspec
import Test.QuickCheck
import Data.List (sort)

import CSVFC (maybeExtract)

main :: IO ()
main = do
  runHspec

runHspec :: IO ()
runHspec = hspec $ do
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

    it "should have one less in the output list of Strings for positive indexes" $ do
      property (prop_outputListHasOneLessThanOriginalForPositiveIntegers :: Int -> [String] -> Bool)


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

