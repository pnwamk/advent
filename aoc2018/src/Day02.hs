module Day02
  ( test
  , run
  ) where

import           Data.Map (Map)
import qualified Data.Map as Map hiding (Map)
import           Test.Hspec
import           Assert

logChar :: Char -> Map Char Int -> Map Char Int
logChar c m = case (Map.lookup c m) of
                Just n -> Map.insert c (n + 1) m
                Nothing -> Map.insert c 1 m

scanId :: String -> (Int, Int)
scanId ident = foldr count (0,0) freqs
  where freqs :: Map Char Int
        freqs = foldr logChar Map.empty ident
        count :: Int -> (Int, Int) -> (Int, Int)
        count 2 (_,b) = (1,b)
        count 3 (a,_) = (a,1)
        count _ acc   = acc

scanIds :: [String] -> (Int, Int)
scanIds idents = foldr scan (0,0) idents
  where scan :: String -> (Int, Int) -> (Int, Int)
        scan ident (a,b) = (a+c, b+d)
          where (c,d) = scanId ident

checksum :: String -> Int
checksum input = twos * threes
  where (twos,threes) = scanIds $ words input

-- are two strings the same except for one char?
isOneOff :: String -> String -> Bool
isOneOff [] [] = False
isOneOff (x:xs) (y:ys)
  | x == y    = isOneOff xs ys
  | otherwise = xs == ys
isOneOff _ _ = False

-- return the common chars between two strings of equal length
common :: String -> String -> String
common [] [] = []
common (x:xs) (y:ys)
  | x == y    = x:(common xs ys)
  | otherwise = common xs ys


findOneOff :: [String] -> Maybe String
findOneOff [] = Nothing
findOneOff (x:xs) =
  case (filter (isOneOff x) xs) of
    [] -> findOneOff xs
    (y:_) -> Just $ common x y

oneOff :: String -> Maybe String
oneOff input = findOneOff $ words input

test :: IO ()
test = hspec $ do
  describe "Day 02 part 1 (simple checksums)" $ do
    it "scanId abcdef" $ do
      scanId "abcdef" `shouldBe` (0,0)
    it "scan bababc" $ do
      scanId "bababc" `shouldBe` (1,1)
    it "scan abbcde" $ do
      scanId "abbcde" `shouldBe` (1,0)
    it "scan abcccd" $ do
      scanId "abcccd" `shouldBe` (0,1)
    it "scan aabcdd" $ do
      scanId "aabcdd" `shouldBe` (1,0)
    it "scan abcdee" $ do
      scanId "abcdee" `shouldBe` (1,0)
    it "scan ababab" $ do
      scanId "ababab" `shouldBe` (0,1)
    it "checksum all above" $ do
      scanIds [ "abcdef"
              , "bababc"
              , "abbcde"
              , "abcccd"
              , "aabcdd"
              , "abcdee"
              , "ababab"]
        `shouldBe` (4,3)

  describe "Day 02 part 2 (find one off)" $ do
    it "one off \"fgij\"" $ do
     findOneOff [ "abcde"
                , "fghij"
                , "klmno"
                , "pqrst"
                , "fguij"
                , "axcye"
                , "wvxyz"]
       `shouldBe` (Just "fgij")


run :: IO ()
run = do
  input <- readFile "input/Day02.txt"
  putStrLn $ assert (checksum input) 5000
  putStrLn $ assert (oneOff input) (Just "ymdrchgpvwfloluktajxijsqb")
