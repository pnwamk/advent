module Day08
  ( test
  , run
  ) where


import           Data.Maybe
import           Test.Hspec
import           Misc

data Node = Node [Node] [Int]
  deriving (Eq, Show, Ord)

-- sums all metadata in the tree
sumTreeMetadata :: Node -> Int
sumTreeMetadata (Node ns ms) = sum $ ms ++ nSums
  where nSums = map sumTreeMetadata ns


nodeVal :: Node -> Int
nodeVal (Node [] ms) = sum ms
nodeVal (Node ns ms) = sum $ mapMaybe val ms
  where nCount = length ns
        val :: Int -> Maybe Int
        val m | m <= 0 || m > nCount = Nothing
              | otherwise            = Just $ nodeVal $ ns !! (m - 1)


parseNodeBody :: Int -> Int -> [Int] -> (Node, [Int])
parseNodeBody nCount mCount initInput = go nCount [] initInput
  where go :: Int -> [Node] -> [Int] -> (Node, [Int])
        go 0 ns input = ((Node (reverse ns) ms), input')
          where (ms, input') = splitAt mCount input
        go c ns (nCount':mCount':input) = go (c - 1) (n:ns) input'
          where (n , input') = parseNodeBody nCount' mCount' input
          


parseTree :: [Int] -> Node
parseTree (c:m:input) = case (parseNodeBody c m input) of
                          (root,[]) -> root
                          (root, _) -> error $
                                       "input to parseTree contained "
                                       ++ "more than one tree!"
parseTree _ = error "invalid input to parseTree"
                             
  

smallInput :: String
smallInput = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"

test = hspec $ do
  describe "Day 08 part 1 (metadata sum)" $ do
    it "sumMetadata" $ do
      (sumTreeMetadata (parseTree (map read (words smallInput))))
        `shouldBe`
        138
        
  describe "Day 08 part 2 (node value)" $ do
    it "nodeVal" $ do
      (nodeVal (parseTree (map read (words smallInput))))
        `shouldBe`
        66


run :: IO ()
run = do
  rawInput <- readFile "input/Day08.txt"
  let input = parseTree $ map read $ words $ rawInput
  assert (sumTreeMetadata input) 41760
  assert (nodeVal input) 25737
