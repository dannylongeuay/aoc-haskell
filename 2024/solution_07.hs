import Data.List (isSuffixOf)
import Text.Printf

part1 :: [(Int, [Int])] -> Int
part1 s = sum [target | (target, nums) <- s, canSolve target nums False]

part2 :: [(Int, [Int])] -> Int
part2 s = sum [target | (target, nums) <- s, canSolve target nums True]

canSolve :: Int -> [Int] -> Bool -> Bool
canSolve target nums withConcat
  | length nums == 1 = target == head nums
  | isDivisible && canSolve nextDividedTarget nextNums withConcat = True
  | greaterThan && canSolve nextSubtractedTarget nextNums withConcat = True
  | withConcat
      && greaterThan
      && targetEndsWithLast
      && canSolve nextConcatTarget nextNums withConcat =
      True
  | otherwise = False
  where
    lastNum = last nums
    nextNums = init nums
    isDivisible = mod target lastNum == 0
    greaterThan = target > lastNum
    nextDividedTarget = div target lastNum
    nextSubtractedTarget = target - lastNum
    targetStr = show target
    lastStr = show lastNum
    targetEndsWithLast = lastStr `isSuffixOf` targetStr
    nextConcatTarget = read $ take (length targetStr - length lastStr) targetStr

parse :: String -> [(Int, [Int])]
parse s = map f $ lines s
  where
    f line = (read $ init $ head $ words line, map read $ tail $ words line)

main :: IO ()
main = do
  s <- getContents
  printf "Part 1: %d\n" $ part1 $ parse s
  printf "Part 2: %d\n" $ part2 $ parse s
