module Day01
  ( test
  , run
  ) where

import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Data.List
import           Test.Hspec
import           Assert

data Cmd = 
    Add Int
  | Sub Int
  deriving Show

parseWords :: [String] -> [Cmd]
parseWords [] = []
parseWords ((sign:num):rst) = 
  case sign of
    '+' -> (Add (read num)):(parseWords rst)
    '-' -> (Sub (read num)):(parseWords rst)
    _ -> error $ "bad sign: " ++ [sign]
parseWords (bad:rst) = error $ "bad token: " ++ bad

runCmd :: Cmd -> Int -> Int
runCmd (Add n) acc = acc + n
runCmd (Sub n) acc = acc - n

runCmds :: [Cmd] -> Int
runCmds cmds = foldr runCmd 0 cmds 

runStr :: String -> Int
runStr input = runCmds $ parseWords $ words input 


findDup :: [Int] -> Maybe Int
findDup ns = go ns Set.empty
  where go :: [Int] -> Set Int -> Maybe Int
        go [] _ = Nothing
        go (x:xs) seen
          | Set.member x seen = Just x
          | otherwise         = go xs (Set.insert x seen)

logCmds :: [Cmd] -> [Int]
logCmds cmds = go cmds 0
  where go :: [Cmd] -> Int -> [Int]
        go [] acc = [acc]
        go (c:cs) acc = acc:(go cs acc')
          where acc' = runCmd c acc

findDupFreq :: String -> Maybe Int
findDupFreq input = findDup $ logCmds $ cs
  where cmds = parseWords $ words input
        cs = cmds ++ cs

test :: IO ()
test = hspec $ do
  describe "Day 01 part 1 (resulting freq)" $ do
  -- compute the result of adding/subtracting
  -- the numbers in the sequence
    it "+1, +1, +1" $ do
      runStr "+1 +1 +1" `shouldBe` 3
    it "+1, +1, -2" $ do
      runStr "+1 +1 -2" `shouldBe` 0
    it "-1, -2, -3" $ do
      runStr "-1 -2 -3" `shouldBe` -6
  describe "Day 01 part 2 (first repeated freq)" $ do
  -- compute the results of adding/subtracting
  -- the numbers at each step in the sequence and
  -- record what frequency appears twice first,
  -- repeating the cycle when necessary
    it "+1, -1" $ do
      findDupFreq "+1 -1" `shouldBe` (Just 0)
    it "+3, +3, +4, -2, -4" $ do
      findDupFreq "+3 +3 +4 -2 -4" `shouldBe` (Just 10)
    it "-6, +3, +8, +5, -6" $ do
      findDupFreq "-6 +3 +8 +5 -6" `shouldBe` (Just 5)
    it "+7, +7, -2, -7, -4" $ do
      findDupFreq "+7 +7 -2 -7 -4" `shouldBe` (Just 14)


run :: IO ()
run = do
  input <- readFile "input/Day01.txt"
  putStrLn $ assert (runStr input) 500
  putStrLn $ assert (findDupFreq input) (Just 709)
