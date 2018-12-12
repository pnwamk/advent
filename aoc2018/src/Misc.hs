module Misc
  ( assert
  , argMax
  , argMin
  , groupOn
  , listDiff
  , listNub
  ) where

import           System.CPUTime
import           Text.Printf
import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set hiding (Set)


assertEq :: (Eq a, Show a) => a -> a -> String
assertEq actual expected
  | actual == expected = "Passed! Answer is "
                         ++ (show expected)
                         ++ "."
  | otherwise = "Failed! Expected "
                ++ (show expected)
                ++ " but got "
                ++ (show actual)
                ++ "."

assert :: (Eq a, Show a) => a -> a -> IO ()
assert actual expected = do
  start <- getCPUTime
  let same = actual == expected
  end <- same `seq` getCPUTime
  let diff = (fromIntegral (end - start)) / (10^12)
  if same
    then do
    putStr $ "Passed! Answer is " ++ (show expected)
    printf " (time %0.3f sec)\n" (diff :: Double)
    else do
    putStrLn
      $ "Failed! Expected "
      ++ (show expected)
      ++ " but got "
      ++ (show actual)
      ++ "."

listDiff :: Ord a => [a] -> [a] -> [a]
listDiff xs ys = filter (\x -> not $ Set.member x ySet) xs
  where ySet = Set.fromList ys

listNub :: Ord a => [a] -> [a]
listNub xs = Set.toList $ Set.fromList xs


argMax :: Ord b => (a -> b) -> [a] -> a
argMax f xs = argOpt (>) f xs
argMin :: Ord b => (a -> b) -> [a] -> a
argMin f xs = argOpt (<) f xs

argOpt :: (b -> b -> Bool) -> (a -> b) -> [a] -> a
argOpt comp f [] = error "argOpt given empty list"
argOpt comp f (x:xs) = go x (f x) xs
  where go y yVal [] = y
        go y yVal (z:zs) =
          let zVal = f z in
            if comp zVal yVal
            then go z zVal zs
            else go y yVal zs

groupOn f = groupBy (\x y -> (f x) == (f y))
