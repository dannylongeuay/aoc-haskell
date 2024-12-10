import Text.Printf

part1 :: String -> Int
part1 s = 0

part2 :: String -> Int
part2 s = 0

main :: IO ()
main = do
  s <- getContents
  printf "Part 1: %d\n" $ part1 s
  printf "Part 2: %d\n" $ part2 s
