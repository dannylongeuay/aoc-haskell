import Data.Map qualified as M
import Data.Maybe
import Text.Printf

type Coord = (Int, Int)

part1 :: M.Map Coord Char -> Int
part1 grid = foldl f 0 $ M.keys grid
  where
    searchWords = ["XMAS", "SAMX"]
    hCheck (x, y) = sum $ map (\w -> if w == map (\i -> fromMaybe '.' $ M.lookup (x, y + i) grid) [0, 1, 2, 3] then 1 else 0) searchWords
    vCheck (x, y) = sum $ map (\w -> if w == map (\i -> fromMaybe '.' $ M.lookup (x + i, y) grid) [0, 1, 2, 3] then 1 else 0) searchWords
    negDiagCheck (x, y) =
      sum $
        map
          ( \w ->
              if w
                == map
                  (\(i, j) -> fromMaybe '.' $ M.lookup (x + i, y + j) grid)
                  [(0, 0), (1, 1), (2, 2), (3, 3)]
                then 1
                else 0
          )
          searchWords
    posDiagCheck (x, y) =
      sum $
        map
          ( \w ->
              if w
                == map
                  (\(i, j) -> fromMaybe '.' $ M.lookup (x + i, y + j) grid)
                  [(0, 0), (-1, 1), (-2, 2), (-3, 3)]
                then 1
                else 0
          )
          searchWords
    f acc c = acc + hCheck c + vCheck c + negDiagCheck c + posDiagCheck c

part2 :: M.Map Coord Char -> Int
part2 grid = foldl f 0 $ M.keys grid
  where
    searchWords = ["MSMS", "SMMS", "SMSM", "MSSM"]
    xCoords = [(-1, -1), (1, 1), (1, -1), (-1, 1)]
    xCheck (x, y) =
      sum $
        map
          ( \w ->
              if 'A' == fromMaybe '.' (M.lookup (x, y) grid)
                && w
                  == map
                    (\(i, j) -> fromMaybe '.' $ M.lookup (x + i, y + j) grid)
                    xCoords
                then 1
                else 0
          )
          searchWords
    f acc c = acc + xCheck c

getGrid :: String -> Coord -> M.Map Coord Char
getGrid [] _ = M.empty
getGrid ('\n' : xs) c = getGrid xs (fst c + 1, 0)
getGrid (x : xs) c = M.insert c x $ getGrid xs (fst c, snd c + 1)

main :: IO ()
main = do
  contents <- readFile "input_04"
  -- contents <- readFile "sample_04"
  let grid = getGrid contents (0, 0)
  printf "Part 1: %d\n" $ part1 grid
  printf "Part 2: %d\n" $ part2 grid
