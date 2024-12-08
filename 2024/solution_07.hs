import Text.Printf

part1 :: [(Int, [Int])] -> Int
part1 s = sum [x | (x, y) <- s, isValid [(+), (*)] x y]

part2 :: [(Int, [Int])] -> Int
part2 s = sum [x | (x, y) <- s, isValid [(+), (*), concatInt] x y]

concatInt :: Int -> Int -> Int
concatInt x y = read (show x ++ show y)

isValid :: [Int -> Int -> Int] -> Int -> [Int] -> Bool
isValid _ _ [] = False
isValid _ x [y] = x == y
isValid ops x (y : z : w) = any isValid' ops
  where
    isValid' op = x >= yz && isValid ops x (yz : w)
      where
        yz = op y z

parse :: String -> [(Int, [Int])]
parse s = map f $ lines s
  where
    f line = (read $ init $ head $ words line, map read $ tail $ words line)

main :: IO ()
main = do
  s <- getContents
  printf "Part 1: %d\n" $ part1 $ parse s
  printf "Part 2: %d\n" $ part2 $ parse s
