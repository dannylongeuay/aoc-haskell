import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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

type Position = (Vector, Int)

type Grid = M.Map Vector Char

getGrid :: String -> Grid
getGrid = getGrid' vOrigin

getGrid' :: Vector -> String -> Grid
getGrid' v [] = M.empty
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

getStart :: Grid -> Position
getStart grid = (fst . head . M.assocs $ M.filter (== 'S') grid, 0)

getGoal :: Grid -> Vector
getGoal = fst . head . M.assocs . M.filter (== 'E')

data World = World {grid :: Grid, start :: Position, goal :: Vector}
  deriving (Show)

getWorld :: String -> World
getWorld s = World {grid, start, goal}
  where
    grid' = getGrid s
    grid = M.filter (== '#') grid'
    start = getStart grid'
    goal = getGoal grid'

moveForward :: Position -> Int -> (Position, Int)
moveForward (v, d) c = ((v .+. (vDirs !! d), d), c + 1)

turnLeft :: Position -> Int -> (Position, Int)
turnLeft (v, d) c = ((v, mod (d - 1) 4), c + 1000)

turnRight :: Position -> Int -> (Position, Int)
turnRight (v, d) c = ((v, mod (d + 1) 4), c + 1000)

nextPositions :: Position -> Int -> [(Position, Int)]
nextPositions r c = [turnLeft r c, moveForward r c, turnRight r c]

data State = State
  { queue :: PQ.MinPQueue Int Position,
    lowestCost :: M.Map Position Int,
    backtrack :: M.Map Position (S.Set Position),
    bestCost :: Int,
    endStates :: S.Set Position
  }

-- heavily modified dijkstra
getState :: World -> State
getState (World grid start goal) = go startingState
  where
    startingState =
      State
        { queue = PQ.singleton 0 start,
          lowestCost = M.singleton start 0,
          backtrack = M.empty,
          bestCost = maxBound,
          endStates = S.empty
        }
    go :: State -> State
    go state@(State pq lc _ bc es)
      -- Reached the end of the queue, return state
      | pq == PQ.Empty = state
      -- Current cost is greater than lowest cost to get to this state, continue
      | c > lowestAtPos = go state {queue = nextQueue}
      -- Reached end with a less optimal path, return state
      | v == goal && c > bc = state
      -- Calculate state changes for neighbors
      | otherwise = go $ foldl goNeighbor nextState (nextPositions p c)
      where
        ((c, p@(v, d)), nextQueue) = PQ.deleteFindMin pq
        lowestAtPos = fromMaybe maxBound $ M.lookup p lc
        (nextBc, nextEs) =
          if v == goal && c <= bc
            then -- Found a better or equal cost at goal. Update bestCost and endStates.
              (c, S.insert p es)
            else (bc, es)
        nextState =
          state
            { queue = nextQueue,
              bestCost = nextBc,
              endStates = nextEs
            }
        goNeighbor :: State -> (Position, Int) -> State
        goNeighbor state'@(State pq' lc' bt' _ _) (p'@(v', d'), c')
          | Just _ <- M.lookup v' grid = state'
          | c' > lowestAtPos' = state'
          | otherwise = nextState'
          where
            nextQueue' = PQ.insert c' p' pq'
            lowestAtPos' = fromMaybe maxBound $ M.lookup p' lc'
            (nextBt', nextLc') =
              if c' < lowestAtPos'
                then -- Found a better cost. Reset backtrack path. Update lowestCost.
                  (M.insert p' (S.singleton p) bt', M.insert p' c' lc')
                else -- No better cost was found. Update backtrack path.
                  (M.insert p' (S.insert p (bt' M.! p')) bt', lc')
            nextState' =
              state'
                { queue = nextQueue',
                  lowestCost = nextLc',
                  backtrack = nextBt'
                }

-- bfs through backtrack paths
getVisited :: State -> S.Set Vector
getVisited (State _ _ bt _ es) = S.map fst $ go (S.elems es) es
  where
    go :: [Position] -> S.Set Position -> S.Set Position
    go [] visited = visited
    go (p@(v, d) : ps) visited
      | Just btPath <- M.lookup p bt = uncurry go $ S.foldl goNeighbor (ps, visited) btPath
      | otherwise = go ps visited
      where
        goNeighbor (q', v') n'
          | S.member n' v' = (q', v')
          | otherwise = (n' : q', S.insert n' v')

part1 :: State -> Int
part1 = bestCost

part2 :: State -> Int
part2 = length . getVisited

main :: IO ()
main = do
  s <- getContents
  let world = getWorld s
  let state = getState world
  printf "Part 1: %d\n" $ part1 state
  printf "Part 2: %d\n" $ part2 state
