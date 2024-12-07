import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Text.Printf

type Vector = (Int, Int)

type Grid = M.Map Vector Char

type Guard = (Vector, Vector)

type Path = S.Set Vector

part1 :: Path -> Int
part1 = length

part2 :: Grid -> Guard -> Path -> Int
part2 grid guard = S.fold f 0
  where
    f v acc
      | loops (addObstacle grid v) (Just guard) S.empty = acc + 1
      | otherwise = acc

addObstacle :: Grid -> Vector -> Grid
addObstacle g v = M.insert v '#' g

guardPath :: Maybe [Guard] -> S.Set Vector
guardPath Nothing = S.empty
guardPath (Just g) = S.fromList $ map fst g

sim :: Grid -> Guard -> Maybe [Guard]
sim grid guard = sequence $ takeWhile (/= Nothing) $ iterate (step grid) (Just guard)

loops :: Grid -> Maybe Guard -> S.Set Guard -> Bool
loops _ Nothing _ = False
loops grid (Just guard) gs
  | S.member guard gs = True
  | otherwise = loops grid (step grid (Just guard)) (S.insert guard gs)

getGrid :: String -> Vector -> Grid
getGrid [] _ = M.empty
getGrid ('\n' : xs) v = getGrid xs (0, snd v + 1)
getGrid (x : xs) v = M.insert v x $ getGrid xs (fst v + 1, snd v)

getGuard :: [(Vector, Char)] -> Guard
getGuard [] = undefined
getGuard ((v, '^') : xs) = (v, (0, -1))
getGuard (_ : xs) = getGuard xs

(.+.) :: Vector -> Vector -> Vector
(.+.) v1 = bimap (fst v1 +) (snd v1 +)

move :: Guard -> Guard
move (v, d) = (v .+. d, d)

turn :: Guard -> Guard
turn (v, d) = (v, (-(snd d), fst d))

step :: Grid -> Maybe Guard -> Maybe Guard
step _ Nothing = Nothing
step grid (Just guard)
  | isNothing obstacle = Nothing
  | obstacle == Just '#' = Just $ turn guard
  | otherwise = Just $ move guard
  where
    v' = move guard
    obstacle = M.lookup (fst v') grid

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s (0, 0)
  let guard = getGuard $ M.assocs grid
  let guardLocations = sim grid guard
  let path = guardPath guardLocations
  printf "Part 1: %d\n" $ part1 path
  printf "Part 2: %d\n" $ part2 grid guard path
