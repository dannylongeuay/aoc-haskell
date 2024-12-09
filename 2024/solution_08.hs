import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Set qualified as S
import Text.Printf

type Vector = (Int, Int)

type Grid = M.Map Vector Char

type Antennas = M.Map Char [Vector]

type Antinodes = S.Set Vector

part1 :: Int -> Int -> Antennas -> Int
part1 w h antennas = length $ foldl f S.empty $ M.elems antennas
  where
    f s vs = S.union (getAntinodes w h vs) s

part2 :: Int -> Int -> Antennas -> Int
part2 w h antennas = length $ foldl f S.empty $ M.elems antennas
  where
    f s vs = S.union (getAntinodesWithResonance w h vs) s

getAntinodes :: Int -> Int -> [Vector] -> Antinodes
getAntinodes w h antennas = S.fromList $ concat [f i j | i <- antennas, j <- antennas, i /= j]
  where
    f v1 v2 = f' v1 v2 ++ f' v2 v1
    f' v1 v2 = filter (inBounds w h) [(v1 .*. 2) .-. v2]

getAntinodesWithResonance :: Int -> Int -> [Vector] -> Antinodes
getAntinodesWithResonance w h antennas = S.fromList $ concat [f i j | i <- antennas, j <- antennas, i /= j]
  where
    f v1 v2 = f' v1 v2 ++ f' v2 v1
    f' v1 v2 = takeWhile (inBounds w h) $ iterate (.-. (v2 .-. v1)) v2

getGrid :: String -> Vector -> Grid
getGrid [] _ = M.empty
getGrid ('\n' : xs) v = getGrid xs (0, snd v + 1)
getGrid ('.' : xs) v = getGrid xs (v .+. (1, 0))
getGrid (x : xs) v = M.insert v x $ getGrid xs (v .+. (1, 0))

getWidthHeight :: String -> (Int, Int)
getWidthHeight s = (length $ head $ lines s, length $ lines s)

(.+.) :: Vector -> Vector -> Vector
(.+.) v1 = bimap (fst v1 +) (snd v1 +)

(.-.) :: Vector -> Vector -> Vector
(.-.) v1 = bimap (fst v1 -) (snd v1 -)

(.*.) :: Vector -> Int -> Vector
(.*.) v1 i = (fst v1 * i, snd v1 * i)

getAntennas :: Grid -> Antennas
getAntennas = M.foldlWithKey f M.empty
  where
    f acc v c = M.insertWith (++) c [v] acc

inBounds :: Int -> Int -> Vector -> Bool
inBounds w h v = fst v >= 0 && fst v < w && snd v >= 0 && snd v < h

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s (0, 0)
  let (w, h) = getWidthHeight s
  let antennas = getAntennas grid
  printf "Part 1: %d\n" $ part1 w h antennas
  printf "Part 2: %d\n" $ part2 w h antennas
