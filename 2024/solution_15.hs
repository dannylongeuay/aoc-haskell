import Data.Map qualified as M
import Text.Printf

type Vector = (Int, Int)

vOrigin :: Vector
vOrigin = (0, 0)

vUp :: Vector
vUp = (0, -1)

vDown :: Vector
vDown = (0, 1)

vLeft :: Vector
vLeft = (-1, 0)

vRight :: Vector
vRight = (1, 0)

(.+.) :: Vector -> Vector -> Vector
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Grid = M.Map Vector Char

data World = World {grid :: Grid, robotPos :: Vector}
  deriving (Show)

splitByEmpty :: String -> (String, String)
splitByEmpty s = go s ""
  where
    go ('\n' : '\n' : rest) acc = (acc, rest)
    go (x : xs) acc = go xs (acc ++ [x])

getGrid :: String -> Grid
getGrid = getGrid' vOrigin

getGrid' :: Vector -> String -> Grid
getGrid' v [] = M.empty
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

widen :: Char -> [Char]
widen '#' = "##"
widen 'O' = "[]"
widen '.' = ".."
widen '@' = "@."
widen '\n' = "\n"

getRobotPos :: Grid -> Vector
getRobotPos = fst . head . M.assocs . M.filter (== '@')

getWorld :: String -> World
getWorld s = World {grid, robotPos}
  where
    grid = M.filter (/= '.') $ getGrid s
    robotPos = getRobotPos grid

getMoves :: String -> [Vector]
getMoves xs = [moveToDir x | x <- xs, x /= '\n']

moveToDir :: Char -> Vector
moveToDir '^' = vUp
moveToDir '>' = vRight
moveToDir 'v' = vDown
moveToDir '<' = vLeft

nextVisits :: (Vector, Char) -> Vector -> [Vector] -> [Vector]
nextVisits (x, c) d q = wideCheck ++ [x .+. d] ++ q
  where
    wideCheck
      | snd d /= 0 && c == '[' = [x .+. vRight]
      | snd d /= 0 && c == ']' = [x .+. vLeft]
      | otherwise = []

sim :: World -> Vector -> World
sim (World grid rPos) d =
  case go M.empty [rPos] of
    Nothing -> World grid rPos
    Just region -> World newGrid (rPos .+. d)
      where
        shiftedRegion = M.mapKeys (d .+.) region
        prunedGrid = M.difference grid region
        newGrid = M.union shiftedRegion prunedGrid
  where
    go visited [] = Just visited
    go visited (q : qs)
      | M.notMember q visited,
        Just c <- M.lookup q grid =
          if c == '#'
            then Nothing
            else
              go
                (M.insert q c visited)
                (nextVisits (q, c) d qs)
      | otherwise = go visited qs

gpsScore :: Int -> Vector -> Char -> Int
gpsScore acc (x, y) c = if c == 'O' || c == '[' then acc + (x + y * 100) else acc

part1 :: World -> [Vector] -> Int
part1 world moves = M.foldlWithKey gpsScore 0 $ grid $ foldl sim world moves

part2 :: World -> [Vector] -> Int
part2 world moves = M.foldlWithKey gpsScore 0 $ grid $ foldl sim world moves

main :: IO ()
main = do
  s <- getContents
  let (input1, input2) = splitByEmpty s
  let world = getWorld input1
  let moves = getMoves input2
  let worldExpanded = getWorld $ concatMap widen input1
  printf "Part 1: %d\n" $ part1 world moves
  printf "Part 2: %d\n" $ part2 worldExpanded moves
