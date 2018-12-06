module Day04
  ( test
  , run
  ) where

import           Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Test.Hspec
import           Assert

class Time a where
   time :: a -> Int

data Event =
  Start Int Int
  | Sleep Int
  | Wake Int
  deriving (Show, Eq)

instance Time Event where
  time (Start t _) = t
  time (Sleep t) = t
  time (Wake t) = t


data Shift = Shift
  { ident :: Int
  , startTime :: Int
  , sleepTimes :: [(Int,Int)] -- inclusive
  }
  deriving (Show, Eq)


parseEvent :: String -> Event
parseEvent input =
  case w1 of
    "Guard" -> Start time $ read w2
    "falls" -> Sleep time
    "wakes" -> Wake time
  where erase :: Char -> Char
        erase c = if elem c "[]-:#" then ' ' else c
        [year,month,day,h,m,w1,w2] = take 7 $ words $ map erase input
        time :: Int
        time = read $ year ++ month ++ day ++ h ++ m
        
        
parseEvents :: String -> [Shift]
parseEvents input = shifts
  where ((Start t n):events) = sortOn time $ map parseEvent $ lines input
        shifts :: [Shift]
        shifts = go events n t []
        go :: [Event] -> Int -> Int -> [(Int,Int)] -> [Shift]
        go [] n t s = [Shift { ident = n
                             , startTime = t
                             , sleepTimes = s}]
        go ((Start t' n'):es) n t s = shift:(go es n' t' [])
          where shift = Shift {ident = n
                              , startTime = t
                              , sleepTimes = s}
        go ((Sleep t1):(Wake t2):es) n t s = (go es n t ((t1,t2):s))
        go _ _ _ es = error $ "invalid event list: " ++ (show es)

        
-- Counts how many square inches have more than one
-- claim which inhabits them.
-- overlap :: [Claim] -> Int
-- overlap cs = length
--              $ filter ((> 1) . length)
--              $ group
--              $ List.sort
--              $ concatMap claimPoints cs

-- Determines which claims do not overlap with any other
-- claims, returning a list of their claimIDs.
-- nonOverlap :: [Claim] -> [Int]
-- nonOverlap cs = Set.toList
--                 $ Set.difference (Set.fromList (map claimID cs))
--                 $ Set.fromList
--                 $ map snd
--                 $ concat
--                 $ filter ((> 1) . length)
--                 $ groupBy (\x y -> (fst x) == (fst y))
--                 $ List.sortOn fst
--                 $ concatMap pointClaimPairs cs
--   where pointClaimPairs c = map (\p -> (p, claimID c)) ps
--           where ps = claimPoints c

example :: String
example = unlines [ "[1518-11-01 00:00] Guard #10 begins shift"
                  , "[1518-11-01 00:05] falls asleep"
                  , "[1518-11-01 00:25] wakes up"
                  , "[1518-11-01 00:30] falls asleep"
                  , "[1518-11-01 00:55] wakes up"
                  , "[1518-11-01 23:58] Guard #99 begins shift"
                  , "[1518-11-02 00:40] falls asleep"
                  , "[1518-11-02 00:50] wakes up"
                  , "[1518-11-03 00:05] Guard #10 begins shift"
                  , "[1518-11-03 00:24] falls asleep"
                  , "[1518-11-03 00:29] wakes up"
                  , "[1518-11-04 00:02] Guard #99 begins shift"
                  , "[1518-11-04 00:36] falls asleep"
                  , "[1518-11-04 00:46] wakes up"
                  , "[1518-11-05 00:03] Guard #99 begins shift"
                  , "[1518-11-05 00:45] falls asleep"
                  , "[1518-11-05 00:55] wakes up"]

test = hspec $ do
  describe "Day 04 part 1 (TBD)" $ do
    it "TBD" $ do
      True
        `shouldBe`
        True
        
  describe "Day 04 part 2 (TBD)" $ do
    it "TBD" $ do
      True
        `shouldBe`
        True


run :: IO ()
run = do
  input <- readFile "input/Day04.txt"
  putStrLn "TODO"
  -- putStrLn $ assert (overlap (parseClaims input)) 103806
  -- putStrLn $ assert (nonOverlap (parseClaims input)) [625]
