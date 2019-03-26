module Day06
  ( test
  , run
  ) where

import           Data.List
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)
import           Test.Hspec
import           Misc

-- the locations of interest
data Node = Node { nX :: Int
                 , nY :: Int}
  deriving (Eq, Ord, Show)

data Point = Point { pX :: Int
                   , pY :: Int}
  deriving (Eq, Ord, Show)

-- manhattan distance between node `n` and point `p`
dist :: Node -> Point -> Int
dist (Node x1 y1) (Point x2 y2) = (abs $ x1 - x2) + (abs $ y1 - y2)

dist' :: Node -> Int -> Int -> Int
dist' (Node x1 y1) x2 y2 = (abs $ x1 - x2) + (abs $ y1 - y2)


-- sum of manhattan distances between nodes in `ns` and point p
totalDist :: [Node] -> Int -> Int -> Int
totalDist ns x y = foldl' (\acc n -> acc + (dist' n x y)) 0 ns


-- Given a non-empty list of nodes, return the points which make up
-- the bounding box (i.e. just the border) that contains (exclusively)
-- all the input points plus `buff` in each direction.
boundingBox :: [Node] -> [Point]
boundingBox ns = [ (Point x y) | x <- [xMin..xMax + 1]
                               , y <- [yMin, yMax]]
                 ++
                 [ (Point x y) | y <- [yMin..xMax + 1]
                               , x <- [xMin, xMax]]
  where xs = map nX ns
        ys = map nY ns
        xMin = (minimum xs) - 1
        xMax = (maximum xs) + 1
        yMin = (minimum ys) - 1
        yMax = (maximum ys) + 1

-- Given a non-empty list of points, return the points which make up
-- the solid box that covers all the input points + `buff` in each direction.
coverBox :: [Node] -> [Point]
coverBox ns = [ (Point x y) | x <- [xMin..xMax + 1]
                            , y <- [yMin..yMax + 1]]
  where xs = map nX ns
        ys = map nY ns
        xMin = minimum xs
        xMax = maximum xs
        yMin = minimum ys
        yMax = maximum ys

-- Return the nodes from 'ns' which would have unbounded areas.
unbounded :: [Node] -> [Node]
unbounded ns = listNub
               $ mapMaybe snd
               $ assign ns
               $ boundingBox ns


-- Return the node from ns that is closest to p.
-- If no point is closest (i.e. they are tied for
-- closest) return None
closest :: [Node] -> Point -> Maybe Node
closest ns p = if (length nearest) == 1
               then Just $ fst $ head nearest
               else Nothing
  where nearest :: [(Node,Int)]
        -- how far is each node from p, sorted by dist
        nearest = head
                  $ groupOn snd
                  $ sortOn snd
                  $ map (\n -> (n, dist n p)) ns

-- Given a list of points of interest (ps) and a list of location
-- points (locs), return the list of pairs (l,p) where l is a loc
-- and p is the closest point to p from ps (if one exists).
assign :: [Node] -> [Point] ->  [(Point, Maybe Node)]
assign ns ps = zip ps $ map (closest ns) ps

-- Calculate all the finite areas for each node,
-- returning the assoc list, node to their nearest points.
finiteAreas :: [Node] -> [(Node,[Point])]
finiteAreas ns = map (\l -> ((fst $ head l), map snd l))
                 $ groupOn fst
                 $ sortOn fst
                 -- drop points with no single closest node
                 -- or whose closes node is an unbounded node.
                 $ mapMaybe dropBad
                 -- pair each point with it's closest node (or None)
                 $ assign ns
                 -- create a box which contains all points
                 -- which will be in the finite areas
                 $ coverBox ns
  where us :: Set Node
        -- the nodes whose area is unbounded
        us = Set.fromList $ unbounded ns
        dropBad :: (Point, Maybe Node) -> Maybe (Node, Point)
        dropBad (_,Nothing) = Nothing
        dropBad (p,Just n)
          | Set.member n us = Nothing
          | otherwise = Just (n,p)

largestArea :: [Node] -> Int
largestArea ns = maximum
                 $ map (length . snd)
                 $ finiteAreas ns

-- return the points whose total accumulative distance from
-- all the nodes is less than `d`
pointsWithin :: Int -> [Node] -> Int
pointsWithin d ns = go xMin yMin 0
  where xs = map nX ns
        ys = map nY ns
        buff = d `div` (length ns)
        xMin = minimum xs - buff
        xMax = maximum xs + buff
        yMin = minimum ys - buff
        yMax = maximum ys + buff
        go :: Int -> Int -> Int -> Int
        go x y acc
          | x > xMax = if y > yMax
                       then acc
                       else go 0 (1 + y) acc
          | (totalDist ns x y) < d = go (1 + x) y (acc + 1)
          | otherwise = go (1 + x) y acc

exNodes :: [Node]
exNodes = [ (Node 1 1)
          , (Node 1 6)
          , (Node 8 3)
          , (Node 3 4)
          , (Node 5 5)
          , (Node 8 9)]
  
test = hspec $ do
  describe "Day 06 part 1 (largest enclosed area)" $ do
    it "largestArea length" $ do
      (largestArea exNodes)
        `shouldBe`
        17
        
  describe "Day 06 part 2 (points within distance from all nodes)" $ do
    it "pointsWithin" $ do
      (pointsWithin 32 exNodes)
        `shouldBe`
        16

parseNode :: String -> Node
parseNode input = Node (read xStr) (read yStr)
  where [xStr, yStr] = words $ map erase input
        erase :: Char -> Char
        erase c = if elem c "," then ' ' else c


run :: IO ()
run = do
  rawInput <- readFile "input/Day06.txt"
  let input = map parseNode $ filter (not . (== "")) $ lines rawInput
  assert (largestArea input) 4166
  assert (pointsWithin 10000 input) 42250
