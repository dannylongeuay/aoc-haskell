import Text.Parsec qualified as P
import Text.Printf

parser :: P.Parsec String () [(Int, Int)]
parser = P.many loop
  where
    loop = P.choice [P.try mulPair, P.try $ P.anyChar >> loop]

parserWithInstructions :: P.Parsec String () [(Int, Int)]
parserWithInstructions = P.many loop
  where
    loop =
      P.choice
        [ P.try mulPair,
          P.try doInstruction,
          P.try dontInstruction,
          P.try $ P.anyChar >> loop
        ]

mulPair :: P.Parsec String () (Int, Int)
mulPair = do
  P.string "mul"
  P.char '('
  digits1 <- P.many1 P.digit
  P.char ','
  digits2 <- P.many1 P.digit
  P.char ')'
  return (read digits1, read digits2)

doInstruction :: P.Parsec String () (Int, Int)
doInstruction = do
  P.string "do"
  P.char '('
  P.char ')'
  return (-1, 0)

dontInstruction :: P.Parsec String () (Int, Int)
dontInstruction = do
  P.string "don't"
  P.char '('
  P.char ')'
  return (0, -1)

pairs :: String -> [(Int, Int)]
pairs s = case P.parse parser "" s of
  Right xs -> xs
  Left _ -> []

pairsWithInstructions :: String -> [(Int, Int)]
pairsWithInstructions s = case P.parse parserWithInstructions "" s of
  Right xs -> xs
  Left _ -> []

filterPairsWithInstructions :: [(Int, Int)] -> [(Int, Int)]
filterPairsWithInstructions [] = []
filterPairsWithInstructions ((0, -1) : xs) = filterPairsWithInstructions $ tail $ dropWhile (\(x, _) -> x /= -1) xs
filterPairsWithInstructions (x : xs) = x : filterPairsWithInstructions xs

part1 :: String -> Int
part1 s = sum $ map (uncurry (*)) $ pairs s

part2 :: String -> Int
part2 s = sum $ map (uncurry (*)) $ filterPairsWithInstructions $ pairsWithInstructions s

main :: IO ()
main = do
  contents <- readFile "input_03"
  -- contents <- readFile "sample_03"
  printf "Part 1: %d\n" $ part1 contents
  printf "Part 2: %d\n" $ part2 contents
