import Data.IntMap qualified as IM
-- import Data.MemoTrie (memo2)
import Text.Printf

type Stones = IM.IntMap Int

part1 :: Stones -> Int
part1 = solve 25

part2 :: Stones -> Int
part2 = solve 75

solve :: Int -> Stones -> Int
solve n stones = sum $ iterate nextStones stones !! n

nextStones :: Stones -> Stones
nextStones stones = IM.fromListWith (+) nextStones'
  where
    nextStones' :: [(Int, Int)]
    nextStones' = concatMap f $ IM.assocs stones
    f (key, count) = map (,count) (applyRule key)

applyRule :: Int -> [Int]
applyRule stone
  | stone == 0 = [1]
  | even width =
      let (l, r) = splitAt (div width 2) (show stone)
       in [read l, read r]
  | otherwise = [2024 * stone]
  where
    width = length (show stone)

-- part1 :: [Int] -> Int
-- part1 = sum . map (`memoSolve` 25)

-- part2 :: [Int] -> Int
-- part2 = sum . map (`memoSolve` 75)

-- memoSolve :: Int -> Int -> Int
-- memoSolve = memo2 memoSolve'

-- memoSolve' :: Int -> Int -> Int
-- memoSolve' _ 0 = 1
-- memoSolve' 0 steps = memoSolve 1 (steps - 1)
-- memoSolve' stone steps
--   | even width =
--       let (l, r) = splitAt (div width 2) (show stone)
--        in memoSolve (read l) (steps - 1) + memoSolve (read r) (steps - 1)
--   where
--     width = length (show stone)
-- memoSolve' stone steps = memoSolve nextStone (steps - 1)
--   where
--     nextStone = stone * 2024

main :: IO ()
main = do
  s <- getContents
  let stones = IM.fromList $ map (\w -> (read w, 1)) $ words s
  -- let stones = map read $ words s
  printf "Part 1: %d\n" $ part1 stones
  printf "Part 2: %d\n" $ part2 stones
