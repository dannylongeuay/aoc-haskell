solution :: [String] -> Integer
solution [] = 0
solution (x : xs) = solution xs + safe x

safe :: String -> Integer
safe s
  | followsRules numbers = 1
  | otherwise = 0
  where
    numbers = map read $ words s

followsRules :: [Integer] -> Bool
followsRules [x, y] = x /= y && abs (x - y) <= 3
followsRules (x : y : xs)
  | x < y = followsAscending (x : y : xs)
  | otherwise = followsDescending (x : y : xs)

followsDescending :: [Integer] -> Bool
followsDescending [x, y] = x > y && x - y <= 3
followsDescending (x : y : xs)
  | x - y > 3 || x <= y = False
  | otherwise = followsDescending (y : xs)

followsAscending :: [Integer] -> Bool
followsAscending [x, y] = x < y && y - x <= 3
followsAscending (x : y : xs)
  | y - x > 3 || x >= y = False
  | otherwise = followsAscending (y : xs)

main :: IO ()
main = do
  contents <- readFile "input_02"
  -- contents <- readFile "sample_02"
  let fileLines = lines contents
  print $ solution fileLines
