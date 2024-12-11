import Data.Bifunctor (first)
import Data.IntMap qualified as IM
import Text.Printf

type Stones = IM.IntMap Int

part1 :: Stones -> Int
part1 = solve 25

part2 :: Stones -> Int
part2 = solve 75

solve :: Int -> Stones -> Int
solve n stones = sum $ last $ take (n + 1) $ iterate nextStones stones

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

main :: IO ()
main = do
  s <- getContents
  let stones = IM.fromList $ map (\w -> (read w, 1)) $ words s
  printf "Part 1: %d\n" $ part1 stones
  printf "Part 2: %d\n" $ part2 stones
