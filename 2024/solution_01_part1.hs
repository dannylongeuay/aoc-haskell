parser :: String -> (Integer, Integer)
parser s = (read (head (words s)), read (words s !! 1))

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (p : xs) = quicksort lesser ++ [p] ++ quicksort greater
  where
    lesser = filter (< p) xs
    greater = filter (>= p) xs

distance :: [Integer] -> [Integer] -> Integer
distance [] [] = 0
distance [xs] [] = 0
distance [] [ys] = 0
distance (x : xs) (y : ys) = distance xs ys + abs (x - y)

main :: IO ()
main = do
  contents <- readFile "input_01"
  -- contents <- readFile "sample_01"
  let fileLines = lines contents
  let listPairs = map parser fileLines :: [(Integer, Integer)]
  let (a, b) = unzip listPairs
  let aSorted = quicksort a
  let bSorted = quicksort b
  print $ distance aSorted bSorted
