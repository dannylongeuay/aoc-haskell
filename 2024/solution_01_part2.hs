import Data.Map qualified as M

parser :: String -> (Integer, Integer)
parser s = (read (head (words s)), read (words s !! 1))

occurrences :: [Integer] -> M.Map Integer Integer
occurrences [] = M.empty
occurrences (x : xs) = M.insert x (M.findWithDefault 0 x current + 1) current
  where
    current = occurrences xs

similarities :: [Integer] -> M.Map Integer Integer -> Integer
similarities xs m =
  foldr (\x -> (+) (x * M.findWithDefault 0 x m)) 0 xs

main :: IO ()
main = do
  contents <- readFile "input_01"
  -- contents <- readFile "sample_01"
  let fileLines = lines contents
  let listPairs = map parser fileLines :: [(Integer, Integer)]
  let (a, b) = unzip listPairs
  let bOccurrences = occurrences b
  print $ similarities a bOccurrences
