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

getRobotPos :: Grid -> Vector
getRobotPos = fst . head . M.assocs . M.filter (== '@')

attemptMove :: Grid -> Vector -> Vector -> [Maybe (Vector, Char)]
attemptMove g v d
  | nextC == '#' = [Nothing]
  | nextC == '.' = [move]
  | otherwise = move : attemptMove g nextV d
  where
    nextV = v .+. d
    nextC = g M.! nextV
    c = g M.! v
    move = Just (nextV, c)

moveRobot :: Char -> Vector -> Grid -> (Grid, Vector)
moveRobot '^' v g = moveRobot' (sequence $ attemptMove g v vUp) v g
moveRobot '>' v g = moveRobot' (sequence $ attemptMove g v vRight) v g
moveRobot 'v' v g = moveRobot' (sequence $ attemptMove g v vDown) v g
moveRobot '<' v g = moveRobot' (sequence $ attemptMove g v vLeft) v g

moveRobot' :: Maybe [(Vector, Char)] -> Vector -> Grid -> (Grid, Vector)
moveRobot' Nothing v g = (g, v)
moveRobot' (Just x) v g = (M.union (M.fromList ((v, '.') : x)) g, fst $ head x)

runRobot :: [Char] -> Grid -> Grid
runRobot moves grid = runRobot' moves robotPos grid
  where
    robotPos = getRobotPos grid

runRobot' :: [Char] -> Vector -> Grid -> Grid
runRobot' [] _ g = g
runRobot' (c : cs) v g = runRobot' cs nextV nextG
  where
    (nextG, nextV) = moveRobot c v g

showGrid :: Grid -> IO ()
showGrid g = do
  let grid =
        concat
          [ g M.! (x, y) : newLine
            | y <- [0 .. 9],
              x <- [0 .. 9],
              let newLine = if x == 9 then "\n" else ""
          ]
  mapM_ print (lines grid)

part1 :: Grid -> String -> Int
part1 startGrid moves = M.foldlWithKey f 0 endGrid
  where
    robotPos = getRobotPos startGrid
    endGrid = runRobot moves startGrid
    f acc (x, y) v = if v == 'O' then acc + x + y * 100 else acc

part2 :: String -> Int
part2 s = 0

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s
  let moves = getMoves s
  printf "Part 1: %d\n" $ part1 grid moves
  printf "Part 2: %d\n" $ part2 s
