import Data.Bifunctor (bimap)
import Data.Map qualified as M
import Data.Maybe (catMaybes)
import Data.Set qualified as S
import Text.Printf

type Vector = (Int, Int)

type Price = (Int, Int)

type Grid = M.Map Vector Char

type Regions = S.Set Region

type Visited = S.Set Vector

type Region = S.Set Vector

type Queue = [Vector]

vUp :: Vector
vUp = (0, -1)

vDown :: Vector
vDown = (0, 1)

vLeft :: Vector
vLeft = (-1, 0)

vRight :: Vector
vRight = (1, 0)

vCardinalDirs :: [Vector]
vCardinalDirs = [vUp, vDown, vLeft, vRight]

vUpLeft :: Vector
vUpLeft = vUp .+. vLeft

vDownLeft :: Vector
vDownLeft = vDown .+. vLeft

vUpRight :: Vector
vUpRight = vUp .+. vRight

vDownRight :: Vector
vDownRight = vDown .+. vRight

vDiagonalDirs :: [Vector]
vDiagonalDirs = [vUpLeft, vUpRight, vDownLeft, vDownRight]

vAllDirs :: [Vector]
vAllDirs = vCardinalDirs ++ vDiagonalDirs

getGrid :: String -> Grid
getGrid = getGrid' (0, 0)

getGrid' :: Vector -> String -> Grid
getGrid' _ [] = M.empty
getGrid' v ('\n' : xs) = getGrid' (0, snd v + 1) xs
getGrid' v (x : xs) = M.insert v x grid
  where
    grid = getGrid' (v .+. vRight) xs

(.+.) :: Vector -> Vector -> Vector
(.+.) v1 = bimap (fst v1 +) (snd v1 +)

validNeighbors :: Grid -> Region -> Vector -> [Vector]
validNeighbors g v q = catMaybes [mV | dir <- vCardinalDirs, mV <- [validNeighbor g v (q .+. dir) c]]
  where
    c = g M.! q

validNeighbor :: Grid -> Region -> Vector -> Char -> Maybe Vector
validNeighbor g v q c = case M.lookup q g of
  Nothing -> Nothing
  Just nC -> if nC == c && S.notMember q v then Just q else Nothing

getRegions :: Grid -> Regions
getRegions g = snd $ M.foldlWithKey f (S.empty, S.empty) g
  where
    f (visited, regions) k _ = case S.member k visited of
      True -> (visited, regions)
      False -> (S.union region visited, S.insert region regions)
        where
          region = getRegion g [k] S.empty

getRegion :: Grid -> Queue -> Visited -> Region
getRegion g [] v = S.empty
getRegion g (q : qs) v = S.insert q $ getRegion g nq nv
  where
    ns = validNeighbors g v q
    nq = ns ++ qs
    nv = S.insert q v

getRegionPrice :: Region -> Int
getRegionPrice r = length r * getRegionSides r

getRegionSides :: Region -> Int
getRegionSides r = sum [1 | dir <- vCardinalDirs, v <- S.elems r, S.notMember (v .+. dir) r]

getRegionPriceDiscount :: Region -> Int
getRegionPriceDiscount r = length r * getRegionCorners r

getRegionCorners :: Region -> Int
getRegionCorners r = sum $ map (getVectorCorners r) $ S.elems r

getVectorCorners :: Region -> Vector -> Int
getVectorCorners r v = getVectorCorners' neighbors
  where
    neighbors = S.fromList [dir | dir <- vAllDirs, let v' = v .+. dir, S.member v' r]

getVectorCorners' :: S.Set Vector -> Int
getVectorCorners' v = length $ filter id [i1, i2, i3, i4, o1, o2, o3, o4]
  where
    -- Inside corners
    i1 = S.disjoint v $ S.fromList [vLeft, vUp]
    i2 = S.disjoint v $ S.fromList [vRight, vUp]
    i3 = S.disjoint v $ S.fromList [vLeft, vDown]
    i4 = S.disjoint v $ S.fromList [vRight, vDown]
    -- Outside corners
    o1 = S.difference (S.fromList [vLeft, vUpLeft, vUp]) v == S.singleton vUpLeft
    o2 = S.difference (S.fromList [vRight, vUpRight, vUp]) v == S.singleton vUpRight
    o3 = S.difference (S.fromList [vLeft, vDownLeft, vDown]) v == S.singleton vDownLeft
    o4 = S.difference (S.fromList [vRight, vDownRight, vDown]) v == S.singleton vDownRight

part1 :: Regions -> Int
part1 = sum . map getRegionPrice . S.elems

part2 :: Regions -> Int
part2 = sum . map getRegionPriceDiscount . S.elems

main :: IO ()
main = do
  s <- getContents
  let grid = getGrid s
  let regions = getRegions grid
  printf "Part 1: %d\n" $ part1 regions
  printf "Part 2: %d\n" $ part2 regions
