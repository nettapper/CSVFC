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

    it "should be the same list if we sort [Int] or we remove on element, prepend it, and sort that" $ do
      property (prop_maybeExtractSortedIfHasOrd :: Int -> [Int] -> Bool)

    it "should be the same list if we sort [String] or we remove on element, prepend it, and sort that" $ do
      property (prop_maybeExtractSortedIfHasOrd :: Int -> [Int] -> Bool)


prop_maybeExtractSortedIfHasOrd :: Ord a => Int -> [a] -> Bool
prop_maybeExtractSortedIfHasOrd i as = (sort as) == (sort as')
  where as' = a' ++ as_rem
        (maybe_a, as_rem) = maybeExtract i as
        a' = case maybe_a of
                  Nothing -> []
                  Just ja -> [ja]

