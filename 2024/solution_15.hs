import Data.Map qualified as M
import Text.Printf

type Vector = (Int, Int)

type Grid = M.Map Vector Char

vUp :: Vector
vUp = (0, -1)

vDown :: Vector
vDown = (0, 1)

vLeft :: Vector
vLeft = (-1, 0)

vRight :: Vector
vRight = (1, 0)

getGrid :: String -> Grid
getGrid = getGrid' (0, 0)

getGrid' :: Vector -> String -> Grid
getGrid' v ('\n' : '\n' : xs) = M.empty
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

(.+.) :: Vector -> Vector -> Vector
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

getMoves :: String -> [Char]
getMoves ('\n' : '\n' : xs) = [x | x <- xs, x /= '\n']
getMoves (x : xs) = getMoves xs

validMove :: Grid -> Vector -> Maybe Vector
validMove g v = Nothing

part1 :: String -> Int
part1 s = 0

part2 :: String -> Int
part2 s = 0

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s
  print grid
  let moves = getMoves s
  print moves
  printf "Part 1: %d\n" $ part1 s
  printf "Part 2: %d\n" $ part2 s
