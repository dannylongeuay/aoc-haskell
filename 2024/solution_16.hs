import Data.Map qualified as M
import Data.PQueue.Prio.Min as PQ
import Data.Set qualified as S
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

vDirs :: [Vector]
vDirs = [vRight, vDown, vLeft, vUp]

(.+.) :: Vector -> Vector -> Vector
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Grid = M.Map Vector Char

data Reindeer = Reindeer {pos :: Vector, dir :: Int, score :: Int}
  deriving (Show)

instance Eq Reindeer where
  Reindeer p1 d1 _ == Reindeer p2 d2 _ = p1 == p2 && d1 == d2

instance Ord Reindeer where
  Reindeer p1 d1 _ <= Reindeer p2 d2 _
    | p1 < p2 = True
    | p1 > p2 = False
    | otherwise = d1 <= d2

type Reindeers = S.Set Reindeer

data World = World {grid :: Grid, reindeer :: Reindeer, goal :: Vector}
  deriving (Show)

getGrid :: String -> Grid
getGrid = getGrid' vOrigin

getGrid' :: Vector -> String -> Grid
getGrid' v [] = M.empty
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

getReindeer :: Grid -> Reindeer
getReindeer grid = Reindeer {pos = fst . head . M.assocs $ M.filter (== 'S') grid, dir = 0, score = 0}

getGoal :: Grid -> Vector
getGoal = fst . head . M.assocs . M.filter (== 'E')

getWorld :: String -> World
getWorld s = World {grid, reindeer, goal}
  where
    grid' = getGrid s
    grid = M.filter (== '#') grid'
    reindeer = getReindeer grid'
    goal = getGoal grid'

moveReindeer :: Reindeer -> Reindeer
moveReindeer (Reindeer p d s) = Reindeer (p .+. (vDirs !! d)) d (succ s)

turnReindeerLeft :: Reindeer -> Reindeer
turnReindeerLeft (Reindeer p d s) = Reindeer p (mod (d - 1) 4) (s + 1000)

turnReindeerRight :: Reindeer -> Reindeer
turnReindeerRight (Reindeer p d s) = Reindeer p (mod (d + 1) 4) (s + 1000)

nextReindeers :: Reindeer -> PQ.MinPQueue Int Reindeer
nextReindeers r = foldl (\acc r' -> PQ.insert (score r') r' acc) PQ.empty [turnReindeerLeft r, moveReindeer r, turnReindeerRight r]

aStarVisited :: World -> Reindeers
aStarVisited (World grid reindeer goal) = go S.empty (PQ.singleton (score reindeer) reindeer)
  where
    go :: Reindeers -> PQ.MinPQueue Int Reindeer -> Reindeers
    go visited PQ.Empty = visited
    go visited pq
      | p == goal = S.insert r visited
      | S.notMember r visited, Nothing <- M.lookup p grid = go (S.insert r visited) (PQ.union (nextReindeers r) rs)
      | otherwise = go visited rs
      where
        p = pos r
        ((_, r), rs) = PQ.deleteFindMin pq

part1 :: World -> Int
part1 world = score $ head $ S.elems $ S.filter (\r -> pos r == goal world) $ aStarVisited world

part2 :: World -> Int
part2 world = 0

main :: IO ()
main = do
  s <- getContents
  let world = getWorld s
  printf "Part 1: %d\n" $ part1 world
  printf "Part 2: %d\n" $ part2 world
