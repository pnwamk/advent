module Assert
  ( assert
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
