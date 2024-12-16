import Data.Map qualified as M
import Text.Printf

data Vector = Vector Int Int
  deriving (Read, Show, Ord, Eq)

xVal :: Vector -> Int
xVal (Vector x _) = x

yVal :: Vector -> Int
yVal (Vector _ y) = y

type Grid = M.Map Vector Char

vUp :: Vector
vUp = Vector 0 (-1)

vDown :: Vector
vDown = Vector 0 1

vLeft :: Vector
vLeft = Vector (-1) 0

vRight :: Vector
vRight = Vector 1 0

getGrid :: String -> Grid
getGrid = getGrid' (Vector 0 0)

getGrid' :: Vector -> String -> Grid
getGrid' v ('\n' : '\n' : xs) = M.empty
getGrid' v ('\n' : xs) = getGrid' (Vector 0 (yVal v + 1)) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

getGridDoubleWidth :: String -> Grid
getGridDoubleWidth = getGridDoubleWidth' (Vector 0 0)

getGridDoubleWidth' :: Vector -> String -> Grid
getGridDoubleWidth' v ('\n' : '\n' : cs) = M.empty
getGridDoubleWidth' v ('\n' : cs) = getGridDoubleWidth' (Vector 0 (yVal v + 1)) cs
getGridDoubleWidth' v (c : cs)
  | c == '#' = M.insert (v .+. vRight) '#' $ M.insert v '#' grid
  | c == 'O' = M.insert (v .+. vRight) ']' $ M.insert v '[' grid
  | c == '@' = M.insert (v .+. vRight) '.' $ M.insert v '@' grid
  | otherwise = M.insert (v .+. vRight) '.' $ M.insert v '.' grid
  where
    grid = getGridDoubleWidth' (v .+. (2 .*. vRight)) cs

(.+.) :: Vector -> Vector -> Vector
(.+.) (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)

(.*.) :: Int -> Vector -> Vector
(.*.) n (Vector x1 y1) = Vector (n * x1) (n * y1)

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

movesToDirs :: String -> [Vector]
movesToDirs [] = []
movesToDirs ('^' : xs) = vUp : movesToDirs xs
movesToDirs ('>' : xs) = vRight : movesToDirs xs
movesToDirs ('v' : xs) = vDown : movesToDirs xs
movesToDirs ('<' : xs) = vLeft : movesToDirs xs

sim :: (Grid, Vector) -> Vector -> (Grid, Vector)
sim (grid, start) d =
  case go M.empty [start] of
    Nothing -> (grid, start)
    Just region -> (grid', start .+. d)
      where
        grid' =
          M.union
            (M.mapKeys (d .+.) region)
            (M.difference grid region)
  where
    go seen [] = Just seen
    go seen (x : xs)
      | M.notMember x seen,
        Just c <- M.lookup x grid =
          if c == '#'
            then Nothing
            else
              go
                (M.insert x c seen)
                ( [x .+. vRight | yVal d /= 0, c == '[']
                    ++ [x .+. vLeft | yVal d /= 0, c == ']']
                    ++ [x .+. d]
                    ++ xs
                )
      | otherwise = go seen xs

part1 :: Grid -> String -> Int
part1 startGrid moves = M.foldlWithKey f 0 endGrid
  where
    robotPos = getRobotPos startGrid
    endGrid = runRobot moves startGrid
    f acc (Vector x y) v = if v == 'O' then acc + x + y * 100 else acc

part2 :: Grid -> [Vector] -> Int
part2 grid moves = M.foldlWithKey f 0 endGrid
  where
    robotPos = getRobotPos grid
    (endGrid, endPos) = foldl sim (grid, robotPos) moves
    f acc (Vector x y) v = if v == '[' then acc + x + y * 100 else acc

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s
  let moves = getMoves s
  let gridDW = M.filter (/= '.') $ getGridDoubleWidth s
  let moveDirs = movesToDirs moves
  printf "Part 1: %d\n" $ part1 grid moves
  printf "Part 2: %d\n" $ part2 gridDW moveDirs
