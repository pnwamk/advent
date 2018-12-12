module Day03
  ( test
  , run
  ) where

import           Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Test.Hspec
import           Misc


data Claim = Claim
  { claimID  :: Int
  , topLeftX :: Int
  , topLeftY :: Int
  , width    :: Int
  , height   :: Int
  }


claimPoints :: Claim -> [(Int, Int)]
claimPoints c = [(x + i, y + j) | i <- [0..w-1], j <- [0..h-1]]
  where (Claim _ x y w h) = c

parseClaim :: String -> Claim
parseClaim input = Claim { claimID  = read n
                         , topLeftX = read x
                         , topLeftY = read y
                         , width    = read w
                         , height   = read h}
  where erase :: Char -> Char
        erase c = if elem c "#@,:x" then ' ' else c
        [n, x, y, w, h] = words $ map erase input
        
parseClaims :: String -> [Claim]
parseClaims input = map parseClaim $ lines input

-- Counts how many square inches have more than one
-- claim which inhabits them.
overlap :: [Claim] -> Int
overlap cs = length
             $ filter ((> 1) . length)
             $ group
             $ List.sort
             $ concatMap claimPoints cs

-- Determines which claims do not overlap with any other
-- claims, returning a list of their claimIDs.
nonOverlap :: [Claim] -> [Int]
nonOverlap cs = listDiff (map claimID cs)
                $ map snd
                $ concat
                $ filter ((> 1) . length)
                $ groupOn fst
                $ List.sortOn fst
                $ concatMap pointClaimPairs cs
  where pointClaimPairs c = map (\p -> (p, claimID c)) ps
          where ps = claimPoints c

test = hspec $ do
  describe "Day 03 part 1 (overlap)" $ do
    it "claimPoints" $ do
      (claimPoints (parseClaim "#3 @ 5,5: 2x2"))
        `shouldBe`
        [(5,5), (5,6), (6,5), (6,6)]
    it "overlap" $ do
      (overlap (map parseClaim [ "#1 @ 1,3: 4x4"
                               , "#2 @ 3,1: 4x4"
                               , "#3 @ 5,5: 2x2"]))
        `shouldBe`
        4
        
  describe "Day 03 part 2 (non-overlapping)" $ do
    it "nonOverlap" $ do
      (nonOverlap (map parseClaim [ "#1 @ 1,3: 4x4"
                                  , "#2 @ 3,1: 4x4"
                                  , "#3 @ 5,5: 2x2"]))
        `shouldBe`
        [3]


run :: IO ()
run = do
  input <- readFile "input/Day03.txt"
  assert (overlap (parseClaims input)) 103806
  assert (nonOverlap (parseClaims input)) [625]
