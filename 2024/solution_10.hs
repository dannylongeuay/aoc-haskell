import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Text.Printf

type Vector = (Int, Int)

type Pos = (Int, Vector)

type Grid = M.Map Vector Int

type Visited = S.Set Vector

type Queue = [Pos]

vUp :: Vector
vUp = (0, -1)

vDown :: Vector
vDown = (0, 1)

vLeft :: Vector
vLeft = (-1, 0)

vRight :: Vector
vRight = (1, 0)

vDirs :: [Vector]
vDirs = [vUp, vDown, vLeft, vRight]

part1 :: String -> Int
part1 s = sum $ map (getTrailheads grid) starts
  where
    (grid, starts) = getGrid s

part2 :: String -> Int
part2 s = sum $ map (getTrailratings grid) starts
  where
    (grid, starts) = getGrid s

getGrid :: String -> (Grid, [Pos])
getGrid = getGrid' (0, 0)

getGrid' :: Vector -> String -> (Grid, [Pos])
getGrid' _ [] = (M.empty, [])
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = (M.insert v h grid, [(0, v) | x == '0'] ++ starts)
  where
    h = read [x]
    (grid, starts) = getGrid' (v .+. vRight) xs

(.+.) :: Vector -> Vector -> Vector
(.+.) v1 = bimap (fst v1 +) (snd v1 +)

getTrailratings :: Grid -> Pos -> Int
getTrailratings g = getTrailratings' g S.empty

getTrailratings' :: Grid -> Visited -> Pos -> Int
getTrailratings' g v p@(pH, pV)
  | pH == 9 = 1
  | otherwise = sum $ map (getTrailratings' g vv) neighbors
  where
    neighbors = validNeighbors g vv p
    vv = S.insert pV v

getTrailheads :: Grid -> Pos -> Int
getTrailheads g p = getTrailheads' g S.empty [p]

getTrailheads' :: Grid -> Visited -> Queue -> Int
getTrailheads' _ _ [] = 0
getTrailheads' g v (p@(pH, pV) : qs) = trailheadInc + getTrailheads' g nextVisited nextQueue
  where
    neighbors = validNeighbors g v p
    nextQueue = neighbors ++ qs
    nextVisited = S.insert pV v
    trailheadInc = if pH == 9 then 1 else 0

validNeighbors :: Grid -> Visited -> Pos -> [Pos]
validNeighbors g v (pH, pV) = catMaybes [mX | dir <- vDirs, mX <- [validNeighbor g v (pH + 1, pV .+. dir)]]

validNeighbor :: Grid -> Visited -> Pos -> Maybe Pos
validNeighbor g v p@(pH, pV) = case M.lookup pV g of
  Nothing -> Nothing
  Just nH -> if pH == nH && S.notMember pV v then Just p else Nothing

main :: IO ()
main = do
  s <- getContents
  let (grid, starts) = getGrid s
  printf "Part 1: %d\n" $ part1 s
  printf "Part 2: %d\n" $ part2 s
