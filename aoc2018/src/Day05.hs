module Day05
  ( test
  , run
  ) where


import           Test.Hspec
import           Data.Char
import           Misc

units :: String
units = ['a'..'z']

removeUnit :: Char -> String -> String
removeUnit u s = filter (not . (== u) . toLower) s

opposite :: Char -> Char -> Bool
opposite a b
  | a == b                     = False
  | (toLower a) == (toLower b) = True
  | otherwise                  = False

-- Reduces the string by repeatedly removing adjacent pairs of letters
-- which are a lower/upper pair e.g. aA, but not aa or AA.
react :: String -> String
react [] = []
react (x:xs) = case (react xs) of
                 [] -> [x]
                 xs'@(y:ys) -> if opposite x y
                               then ys
                               else (x:xs')

-- Calculates which letter of the alphabet, if removed, would produce
-- the smallest "reacted" string.
bestRemove :: String -> Char
bestRemove input = argMin go units
  where go :: Char -> Int
        go u = length $ react $ removeUnit u input

bestRemoveLen :: String -> Int
bestRemoveLen input = length $ react $ removeUnit (bestRemove input) input

test = hspec $ do
  describe "Day 05 part 1 (polymer reaction)" $ do
    it "example reaction 1" $ do
      react "dabAcCaCBAcCcaDA"
        `shouldBe`
        "dabCBAcaDA"
    it "example reaction 2" $ do
      react "dabAcCaCBACcCcaDA"
        `shouldBe`
        "dabCBDA"
        
  describe "Day 05 part 2 (removing a unit)" $ do
    it "best remove" $ do
      bestRemove "dabAcCaCBAcCcaDA"
        `shouldBe`
        'c'
    it "best remove length" $ do
      bestRemoveLen "dabAcCaCBAcCcaDA"
        `shouldBe`
        4

run :: IO ()
run = do
  rawInput <- readFile "input/Day05.txt"
  let input = head $ words rawInput
  putStrLn $ assert (length (react input)) 10804
  putStrLn $ assert (bestRemoveLen input) 6650
