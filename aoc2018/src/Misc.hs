module Misc
  ( assert
  , argMax
  , argMin
  ) where


assert :: (Eq a, Show a) => a -> a -> String
assert actual expected
  | actual == expected = "Passed! Answer is "
                         ++ (show expected)
                         ++ "."
  | otherwise = "Failed! Expected "
                ++ (show expected)
                ++ " but got "
                ++ (show actual)
                ++ "."


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
