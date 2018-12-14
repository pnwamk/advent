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

-- part 1
data Step = Step { letter :: Char
                 , prereqs :: [Char]}
  deriving (Eq, Show)
-- order steps by prereq count and then by letter
instance Ord Step where
  compare (Step c1 p1) (Step c2 p2) =
    case compare (length p1) (length p2) of
      LT -> LT
      GT -> GT
      EQ -> compare c1 c2
  
updateStep :: Char -> Step -> Step
updateStep c (Step c' p) = (Step c' (delete c p))

updateSteps :: Char -> [Step] -> [Step]
updateSteps c ss = sort $ map (updateStep c) ss

orderSteps :: [Step] -> String
orderSteps steps = go $ sort steps
  where go [] = ""
        go ((Step c _):ss) = c:(orderSteps $ updateSteps c ss)

-- Parses the input and returns the mapping from Chars to the set of
-- Chars which must come before that Char.
parseInput :: String -> [Step]
parseInput input = Map.foldrWithKey toStepList [] edgeMap
  where toStepList :: Char -> Set Char -> [Step] -> [Step]
        toStepList c cs ss = (Step c $ Set.toList cs):ss
        edgeMap :: Map Char (Set Char)
        edgeMap = Map.fromListWith Set.union
                  $ (map (\(c1,c2) -> (c1, Set.singleton c2)) edges
                     ++ zip letters (repeat Set.empty))
        edges :: [(Char, Char)]
        edges = mapMaybe parseLine $ lines input
        letters :: [Char]
        letters = listNub $ uncurry (++) $ unzip edges
        parseLine :: String -> Maybe (Char, Char)
        parseLine ws =
          case words ws of
            [] -> Nothing
            -- Step `c1` must be finished before step `c2` can begin.
            [_, [c1], _, _, _, _, _, [c2], _, _] -> Just (c2, c1)
            _ -> error $ "invalid line: " ++ ws


-- part 2
-- how many workers?
wCount = 5
letterNum :: Map Char Int
letterNum = Map.fromList $ zip ['A'..'Z'] [1..]
letterTime :: Char -> Int
letterTime l = 60 + (letterNum Map.! l)


tic :: [(Char, Int)] -> [(Char, Int)]
tic xs = map (\(c,n) -> (c, n-1)) xs
addJob :: Char -> [(Char, Int)] -> [(Char, Int)]
addJob c js = sortOn snd $ (c, (letterTime c)):js

timeWork :: [Step] -> Int
timeWork orig = go (sort orig) wCount []
  where go :: [Step] -> Int -> [(Char, Int)] -> Int
        go [] free [] = 0
        go ss free ((c,0):jobs) = go (updateSteps c ss) (free + 1) jobs
        go ss 0 js = 1 + (go ss 0 $ tic js)
        go ((Step c []):ss) free jobs = go ss (free - 1) (addJob c jobs)
        go ss free jobs = 1 + (go ss free $ tic jobs)
        
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
        
  describe "Day 07 part 2 (TBD)" $ do
    it "pointsWithin" $ do
      timeWork (parseInput smallInput)
        `shouldBe`
        253

run :: IO ()
run = do
  rawInput <- readFile "input/Day07.txt"
  let input = parseInput $ rawInput
  assert (orderSteps input) "CHILFNMORYKGAQXUVBZPSJWDET"
  assert (timeWork input) 42
