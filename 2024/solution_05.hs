import Data.List
import Data.Map qualified as M
import Data.Tuple (swap)
import Text.Printf

type Pair = (Int, Int)

part1 :: M.Map Pair Bool -> [[Int]] -> Int
part1 rules pages = sum $ map (\i -> i !! div (length i) 2) $ filter isSorted pages
  where
    sortByRules = sortBy $ ruleCompare rules
    isSorted page = page == sortByRules page

part2 :: M.Map Pair Bool -> [[Int]] -> Int
part2 rules pages = sum $ map (\i -> sortByRules i !! div (length i) 2) $ filter (not . isSorted) pages
  where
    sortByRules = sortBy $ ruleCompare rules
    isSorted page = page == sortByRules page

ruleCompare :: M.Map Pair Bool -> Int -> Int -> Ordering
ruleCompare m a b = case M.lookup (a, b) m of
  Just True -> LT
  Just False -> GT
  Nothing -> EQ

rules :: String -> M.Map Pair Bool
rules s = M.fromList $ concatMap f $ lines s
  where
    f line = [(parseRule line, True), (swap $ parseRule line, False)]

pages :: String -> [[Int]]
pages s = map page $ lines s

page :: String -> [Int]
page [] = []
page s = case dropWhile (== ',') s of
  "" -> []
  s' -> read p : page s''
    where
      (p, s'') = break (== ',') s'

parseRule :: String -> Pair
parseRule s = (read $ take 2 s, read $ drop 3 s)

parseInput :: String -> (M.Map Pair Bool, [[Int]])
parseInput s = (rules $ parseInput' s, pages $ parseInput'' s)

parseInput' :: String -> String
parseInput' [] = []
parseInput' (x : xs)
  | [x, head xs] == "\n\n" = parseInput' ""
  | otherwise = x : parseInput' xs

parseInput'' :: String -> String
parseInput'' [] = []
parseInput'' (x : xs)
  | [x, head xs] == "\n\n" = tail xs
  | otherwise = parseInput'' xs

main :: IO ()
main = do
  s <- getContents
  let (r, p) = parseInput s
  printf "Part 1: %d\n" $ part1 r p
  printf "Part 2: %d\n" $ part2 r p
