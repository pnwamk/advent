module Day07
  ( test
  , run
  ) where

import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map hiding (Map)
import           Test.Hspec
import           Misc

type ID = Char

-- part 1
data Step = Step { stepID :: ID
                 , stepPrereqs :: Set ID}
  deriving (Eq, Show)
-- order steps by prereq count and then by letter
instance Ord Step where
  compare (Step c1 p1) (Step c2 p2) =
    case compare (Set.size p1) (Set.size p2) of
      LT -> LT
      GT -> GT
      EQ -> compare c1 c2

--update a step by noting one job ID is done
updateStep :: ID -> Step -> Step
updateStep i (Step i' p) = (Step i' (Set.delete i p))

updateSteps :: ID -> [Step] -> [Step]
updateSteps i [] = []
updateSteps i ss = m:(delete m ss')
  where ss' = map (updateStep i) ss
        m   = minimum ss'

orderSteps :: [Step] -> String
orderSteps steps = go $ sort steps
  where go [] = ""
        go ((Step c _):ss) = c:(orderSteps $ updateSteps c ss)

-- Parses the input and returns the mapping from Chars to the set of
-- Chars which must come before that Char.
parseInput :: String -> [Step]
parseInput input = Map.foldrWithKey toStepList [] edgeMap
  where toStepList :: ID -> Set ID -> [Step] -> [Step]
        toStepList c cs ss = (Step c $ cs):ss
        edgeMap :: Map ID (Set ID)
        edgeMap = Map.fromListWith Set.union
                  $ (map (\(i1,i2) -> (i1, Set.singleton i2)) edges
                     ++ zip letters (repeat Set.empty))
        edges :: [(ID, ID)]
        edges = mapMaybe parseLine $ lines input
        letters :: [ID]
        letters = listNub $ uncurry (++) $ unzip edges
        parseLine :: String -> Maybe (ID, ID)
        parseLine ws =
          case words ws of
            [] -> Nothing
            -- Step `i1` must be finished before step `i2` can begin.
            [_, [i1], _, _, _, _, _, [i2], _, _] -> Just (i2, i1)
            _ -> error $ "invalid line: " ++ ws


-- part 2
-- how many workers?
wCount = 5
idNum :: Map Char Int
idNum = Map.fromList $ zip ['A'..'Z'] [1..]
idTime :: Char -> Int
idTime l = 60 + (idNum Map.! l)

data Job = Job { jobID :: ID
               , jobTime :: Int}
  deriving (Eq, Show)
-- order steps by prereq count and then by letter
instance Ord Job where
  compare (Job i1 t1) (Job i2 t2) = compare t1 t2


addJob :: ID -> Set Job -> Int -> Set Job
addJob i js t = Set.insert (Job i (t + (idTime i))) js

jobDone :: Set Job -> Int -> Bool
jobDone js t
  | Set.null js = False
  | otherwise   = t >= (jobTime $ Set.findMin js)

stepReady :: [Step] -> Bool
stepReady []              = False
stepReady ((Step _ ps):_) = Set.null ps

remStep :: [Step] -> [Step]
remStep (_:[]) = []
remStep (_:ss) = m:(delete m ss)
  where m = minimum ss

timeWork :: [Step] -> Int
timeWork orig = go (sort orig) wCount Set.empty 0
  where go :: [Step] -> Int -> Set Job -> Int -> Int
        -- INVARIANT: (steps == []) || ((head steps) == (minimum steps))
        go steps free jobs time
          -- Are we done? If so return the current time.
          | steps == [] && Set.null jobs
          = time
          -- Is there a finished job? If so update the worker
          -- pool and prereqs of the waiting steps,
          | jobDone jobs time
          = let steps' = updateSteps (jobID (Set.findMin jobs)) steps
                free'  = free + 1
                jobs'  = Set.deleteMin jobs in
              go steps' free' jobs' time
          -- Is there a step ready to be started and a free worker?
          -- If so give them the job!
          | stepReady steps && free > 0
          = let steps' = remStep steps
                free'  = free - 1
                jobs'  = addJob (stepID $ head steps) jobs time in
              go steps' free' jobs' time
          | otherwise
          -- otherwise we're not done and there's nothing to update,
          -- tic time.
          = go steps free jobs (time + 1)
        
smallInput :: String
smallInput = unlines [ "Step C must be finished before step A can begin."
                     , "Step C must be finished before step F can begin."
                     , "Step A must be finished before step B can begin."
                     , "Step A must be finished before step D can begin."
                     , "Step B must be finished before step E can begin."
                     , "Step D must be finished before step E can begin."
                     , "Step F must be finished before step E can begin."]
  
test = hspec $ do
  describe "Day 07 part 1 (step ordering)" $ do
    it "stepOrd" $ do
      (orderSteps (parseInput smallInput))
        `shouldBe`
        "CABDFE"
        
  describe "Day 07 part 2 (timing work with many workers)" $ do
    it "timeWork" $ do
      timeWork (parseInput smallInput)
        `shouldBe`
        253

run :: IO ()
run = do
  rawInput <- readFile "input/Day07.txt"
  let input = parseInput $ rawInput
  assert (orderSteps input) "CHILFNMORYKGAQXUVBZPSJWDET"
  assert (timeWork input) 891
