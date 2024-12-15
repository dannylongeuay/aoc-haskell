import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Set qualified as S
import Text.Printf

type Vector = (Int, Int)

(.+.) :: Vector -> Vector -> Vector
(.+.) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

(.*.) :: Int -> Vector -> Vector
(.*.) n (x1, y1) = (n * x1, n * y1)

data Robot = Robot {pos :: Vector, vel :: Vector}
  deriving (Show)

parse :: String -> [Robot]
parse s = map parseLine $ lines s

parseLine :: String -> Robot
parseLine s = Robot (px1, py1) (vx1, vy1)
  where
    [pRaw, vRaw] = words s
    (pLSplit, pRSplit) = break (== ',') pRaw
    (vLSplit, vRSplit) = break (== ',') vRaw
    px1 = read $ drop 2 pLSplit
    py1 = read $ drop 1 pRSplit
    vx1 = read $ drop 2 vLSplit
    vy1 = read $ drop 1 vRSplit

stepN :: Int -> Robot -> Robot
stepN n (Robot pos vel) = Robot (pos .+. (n .*. vel)) vel

modRobot :: Int -> Int -> Robot -> Robot
modRobot w h (Robot (x, y) vel) = Robot (x `mod` w, y `mod` h) vel

quadrantCounts :: Int -> Int -> [Robot] -> (Int, Int, Int, Int)
quadrantCounts _ _ [] = (0, 0, 0, 0)
quadrantCounts w h ((Robot (x, y) _) : rs) = (nextUL, nextUR, nextDL, nextDR)
  where
    (uL, uR, dL, dR) = quadrantCounts w h rs
    nextUL = if x < w `div` 2 && y < h `div` 2 then uL + 1 else uL
    nextUR = if x > w `div` 2 && y < h `div` 2 then uR + 1 else uR
    nextDL = if x < w `div` 2 && y > h `div` 2 then dL + 1 else dL
    nextDR = if x > w `div` 2 && y > h `div` 2 then dR + 1 else dR

part1 :: [Robot] -> Int
part1 rs = uL * uR * dL * dR
  where
    w = 101
    h = 103
    (uL, uR, dL, dR) = quadrantCounts w h $ map (modRobot w h . stepN 100) rs

part2 :: [Robot] -> Int
part2 _ = 7286

visualize :: [Robot] -> IO ()
visualize rs = do
  let rSet = S.fromList $ map (\(Robot pos _) -> pos) rs
  let grid =
        concat
          [ s ++ newLine
            | x <- [22 .. 54],
              y <- [20 .. 55],
              let newLine = if y == 55 then "\n" else "",
              let s = if S.member (x, y) rSet then "R" else "."
          ]
  mapM_ print (lines grid)

visualizeN :: Int -> Int -> [Robot] -> IO ()
visualizeN start end _
  | start == end = return ()
visualizeN n end rs = do
  threadDelay 200000
  visualize newRs
  printf "Step %d\n" nextStepN
  visualizeN (n + 1) end rs
  where
    nextStepN = n * 103 + 76
    newRs = map (modRobot 101 103 . stepN nextStepN) rs

main :: IO ()
main = do
  s <- getContents
  let robots = parse s
  -- visualizeN 1 100 robots
  visualize $ map (modRobot 101 103 . stepN 7286) robots
  printf "Part 1: %d\n" $ part1 robots
  printf "Part 2: %d\n" $ part2 robots
