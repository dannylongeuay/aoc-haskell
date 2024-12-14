import Data.Char (isDigit)
import Data.List (partition)
import Data.Maybe (fromMaybe)
import Text.Printf

type Vector = (Int, Int)

type Tokens = Int

data ClawMachine = CW {buttonA :: Vector, buttonB :: Vector, prize :: Vector}
  deriving (Show)

aCost :: Tokens
aCost = 3

bCost :: Tokens
bCost = 1

part1 :: [ClawMachine] -> Tokens
part1 cws = sum $ map (fromMaybe 0 . maybeTokens) cws

part2 :: [ClawMachine] -> Tokens
part2 cws = sum $ map ((fromMaybe 0 . maybeTokens) . updateCost) cws
  where
    updateCost (CW a b (px, py)) = CW a b (px + 10000000000000, py + 10000000000000)

parse :: String -> [ClawMachine]
parse s = parse' $ lines s
  where
    parse' [] = []
    parse' ([] : rest) = parse' rest
    parse' (a : b : c : rest) = CW aV bV cV : parse' rest
      where
        aV = (f $ last $ init $ words a, f $ last $ words a)
        bV = (f $ last $ init $ words b, f $ last $ words b)
        cV = (f $ last $ init $ words c, f $ last $ words c)
        f w = read $ fst $ partition isDigit w

-- Two system equation
-- ax * a + bx * b = px
-- ay * a + by * b = py

-- Isolate variables
-- b = (py - ay * a) / by
-- a = (px - bx * b) / ax

-- Solve for b with substitution method
-- b = (py - ay * (px - bx * b) / ax) / by
-- b * by = (py - ay * (px - bx * b) / ax)
-- b * by * ax = py * ax - ay * (px - bx * b)
-- b * by * ax = py * ax - ay * px + ay * bx * b
-- b * by * ax - ay * bx * b = py * ax - ay * px
-- b * (by * ax - ay * bx) = py * ax - ay * px

-- Equations to solve for
-- b = (py * ax - ay * px) / (by * ax - ay * bx)
-- a = (px - bx * b) / ax

maybeTokens :: ClawMachine -> Maybe Tokens
maybeTokens (CW (ax, ay) (bx, by) (px, py))
  | (rm == 0) && (rn == 0) = Just (aCost * n + bCost * m)
  | otherwise = Nothing
  where
    (m, rm) = (py * ax - ay * px) `quotRem` (by * ax - ay * bx)
    (n, rn) = (px - m * bx) `quotRem` ax

main :: IO ()
main = do
  s <- getContents
  let cws = parse s
  printf "Part 1: %d\n" $ part1 cws
  printf "Part 2: %d\n" $ part2 cws
