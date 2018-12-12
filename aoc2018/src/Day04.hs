module Day04
  ( test
  , run
  ) where

import           Data.List as List
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Test.Hspec
import           Misc

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

shiftSleep :: Shift -> Int
shiftSleep s = foldr (+) 0 $ map (\(t1,t2)-> t2 - t1) $ sleepTimes s

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
        go ((Sleep t1):(Wake t2):es) n t s = (go es n t ((t1',t2'):s))
          where t1' = t1 `mod` 100
                t2' = t2 `mod` 100
        go _ _ _ es = error $ "invalid event list: " ++ (show es)
  

-- Reports which guard slept the most
sleepiest :: [Shift] -> Int
sleepiest input = ident $ head $ snd $ argMax fst
                  $ map sumShifts
                  $ groupOn ident
                  $ sortOn ident input
  where sumShifts :: [Shift] -> (Int, [Shift])
        sumShifts ss = (total, ss)
          where total = foldr (+) 0 $ map shiftSleep ss


shiftSleepMins :: Shift -> [Int]
shiftSleepMins s = concat segs
  where segs = [[i..j-1] | (i,j) <- (sleepTimes s)]

-- For guard `n`, report what minute they sleep the most.
minute :: Int -> [Shift] -> Int
minute n shifts = head
                  $ argMax length
                  $ group
                  $ sort
                  $ concatMap shiftSleepMins
                  $ filter ((== n) . ident) shifts

reportSleepy :: String -> Int
reportSleepy input = guard * m
  where shifts = parseEvents input
        guard  = sleepiest shifts
        m      = minute guard shifts


-- Takes a list of shifts and repots which minute
-- was slept the most and how many times that minute
-- was slept
sleepiestMin :: [Shift] -> (Int, Int)
sleepiestMin shifts = (\l -> (head l, length l))
                      $ argMax length
                      $ group
                      $ sort
                      $ concatMap shiftSleepMins shifts

-- Which guard is asleep most often on the same minute?
sameMinute :: [Shift] -> (Int, Int)
sameMinute shifts = (\x -> (snd x, fst $ fst x))
                    $ argMax (snd . fst)
                    $ map (\ss -> (sleepiestMin ss, ident $ head ss))
                    $ groupBy (\s1 s2 -> (ident s1) == (ident s2))
                    $ sortOn ident
                    $ filter (not . (== []) . sleepTimes) shifts

reportSameMinute :: String -> Int
reportSameMinute input = guard * m
  where shifts = parseEvents input
        (guard, m)  = sameMinute shifts
  
exEvents :: String
exEvents = unlines [ "[1518-11-01 00:00] Guard #10 begins shift"
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
    it "sleepiest" $ do
      (sleepiest (parseEvents exEvents)) 
        `shouldBe`
        10
    it "minute" $ do
      let es = (parseEvents exEvents) in
        (minute (sleepiest es) es)
        `shouldBe`
        24
    it "report" $ do
        reportSleepy exEvents
        `shouldBe`
        240
        
  describe "Day 04 part 2 (TBD)" $ do
    it "sleepiest minute" $ do
      sameMinute (parseEvents exEvents)
        `shouldBe`
        (99,45)
  it "reportSameMinute" $ do
    reportSameMinute exEvents
      `shouldBe`
      4455

run :: IO ()
run = do
  input <- readFile "input/Day04.txt"
  assert (reportSleepy input) 119835
  assert (reportSameMinute input) 12725
