import Data.List (sortOn)
import Data.Map qualified as M
import Data.Maybe (catMaybes, isJust)
import Text.Printf

part1 :: String -> Int
part1 s = checksumBlocks $ compactBlocks (length reverseFiltered) expanded reverseFiltered
  where
    expanded = expandBlocks 0 True s
    reverseFiltered = reverse $ filter isJust expanded

part2 :: String -> Int
part2 s = checksumFiles compactedFiles
  where
    (files, spaces) = parseInput s
    spacesMap = M.fromList spaces
    compactedFiles = sortOn (fst . snd) $ compactFiles (reverse files) spacesMap

expandBlocks :: Int -> Bool -> String -> [Maybe Int]
expandBlocks _ _ ['\n'] = []
expandBlocks id isFile (x : xs)
  | isFile = replicate n (Just id) ++ expandBlocks nextId False xs
  | otherwise = replicate n Nothing ++ expandBlocks id True xs
  where
    n = read [x]
    nextId = id + 1

compactBlocks :: Int -> [Maybe Int] -> [Maybe Int] -> [Maybe Int]
compactBlocks 0 _ _ = []
compactBlocks target (Nothing : xs) (y : ys) = y : compactBlocks (target - 1) xs ys
compactBlocks target ((Just x) : xs) ys = Just x : compactBlocks (target - 1) xs ys

checksumBlocks :: [Maybe Int] -> Int
checksumBlocks = sumOfProducts . catMaybes
  where
    sumOfProducts = sum . zipWith (*) [0 ..]

parseInput :: String -> ([(Int, (Int, Int))], [(Int, (Int, Int))])
parseInput s = parseInput' s 0 0 0

parseInput' :: String -> Int -> Int -> Int -> ([(Int, (Int, Int))], [(Int, (Int, Int))])
parseInput' ['\n'] _ _ _ = ([], [])
parseInput' (sizeStr : xs) pos evenId oddId = (foo : evens, odds)
  where
    sizeNum = read [sizeStr]
    foo = (evenId, (pos, sizeNum))
    (odds, evens) = parseInput' xs (pos + sizeNum) (oddId + 1) evenId

nextValidSpace :: [(Int, (Int, Int))] -> (Int, Int) -> Maybe (Int, Int)
nextValidSpace [] _ = Nothing
nextValidSpace ((sid, (sPos, sSize)) : xs) (cPos, cSize)
  | cPos < sPos = Nothing
  | cSize <= sSize = Just (sid, sPos)
  | otherwise = nextValidSpace xs (cPos, cSize)

updateSpaces :: Int -> Int -> M.Map Int (Int, Int) -> M.Map Int (Int, Int)
updateSpaces fSize sid idSpaces = case M.lookup sid idSpaces of
  Nothing -> idSpaces
  Just (sPos, sSize) ->
    ( if fSize == sSize
        then M.delete sid idSpaces
        else M.insert sid (sPos + fSize, sSize - fSize) idSpaces
    )

compactFiles :: [(Int, (Int, Int))] -> M.Map Int (Int, Int) -> [(Int, (Int, Int))]
compactFiles [] _ = []
compactFiles ((sid, f@(pos, size)) : xs) idSpaces = case nextValidSpace (M.assocs idSpaces) f of
  Nothing -> (sid, f) : compactFiles xs idSpaces
  Just (xid, xPos) -> (sid, (xPos, size)) : compactFiles xs (updateSpaces size xid idSpaces)

checksumFiles :: [(Int, (Int, Int))] -> Int
checksumFiles [] = 0
checksumFiles ((sid, (pos, size)) : xs) = x + checksumFiles xs
  where
    x = sum $ take size (map (uncurry (*) . (,sid)) [pos ..])

main :: IO ()
main = do
  s <- getContents
  printf "Part 1: %d\n" $ part1 s
  printf "Part 2: %d\n" $ part2 s
