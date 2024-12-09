import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Text.Printf

type Vector = (Int, Int)

type Grid = M.Map Vector Char

part1 :: String -> Int
part1 s = 0

part2 :: String -> Int
part2 s = 0

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

getFreqs :: Grid -> M.Map Char [Vector]
getFreqs = getFreqs' . M.assocs

getFreqs' :: [(Vector, Char)] -> M.Map Char [Vector]
getFreqs' [] = M.empty
getFreqs' ((v, c) : xs) = M.insertWith (++) c [v] $ getFreqs' xs

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s (0, 0)
  let (w, h) = getWidthHeight s
  printf "Part 1: %d\n" $ part1 s
  printf "Part 2: %d\n" $ part2 s
