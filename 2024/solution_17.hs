import Control.Monad (guard)
import Data.Bits (shiftL, shiftR, xor)
import Data.Char (isDigit)
import Data.List (partition)
import Text.Printf

splitByEmpty :: String -> (String, String)
splitByEmpty s = go s ""
  where
    go ('\n' : '\n' : rest) acc = (acc, rest)
    go (x : xs) acc = go xs (acc ++ [x])

type Registers = (Int, Int, Int)

type Program = ([(Int, Int)], Int, [Int])

parseRegisters :: String -> Registers
parseRegisters s = (getInt lineA, getInt lineB, getInt lineC)
  where
    [lineA, lineB, lineC] = lines s
    getInt = read . last . words

parseProgram :: String -> Program
parseProgram s = (go digits, 0, [])
  where
    (digits, _) = partition isDigit s
    go [] = []
    go (x : y : zs) = (read [x], read [y]) : go zs

combo :: Int -> Registers -> Int
combo 4 (regA, _, _) = regA
combo 5 (_, regB, _) = regB
combo 6 (_, _, regC) = regC
combo x _ = x

normalizePointer :: Int -> Int
normalizePointer x = x `div` 2

expandInstructions :: [(Int, Int)] -> [Int]
expandInstructions [] = []
expandInstructions ((x, y) : zs) = x : y : expandInstructions zs

run :: Registers -> Program -> [Int]
run regs prog = output
  where
    (_, (_, _, output), _) = until isHalted step (regs, prog, False)
    isHalted (_, _, halted) = halted

step :: (Registers, Program, Bool) -> (Registers, Program, Bool)
step (regs@(regA, regB, regC), prog@(instructions, pointer, output), _)
  -- halt
  | np >= length instructions = (regs, prog, True)
  -- adv
  | opc == 0 = ((shiftR regA co, regB, regC), (instructions, nextPointer, output), False)
  -- bxl
  | opc == 1 = ((regA, xor regB lo, regC), (instructions, nextPointer, output), False)
  -- bst
  | opc == 2 = ((regA, co `mod` 8, regC), (instructions, nextPointer, output), False)
  -- jnz
  | opc == 3 =
      if regA == 0
        then ((regA, regB, regC), (instructions, nextPointer, output), False)
        else ((regA, regB, regC), (instructions, lo, output), False)
  -- bxc
  | opc == 4 = ((regA, xor regB regC, regC), (instructions, nextPointer, output), False)
  -- out
  | opc == 5 = ((regA, regB, regC), (instructions, nextPointer, output ++ [co `mod` 8]), False)
  -- bdv
  | opc == 6 = ((regA, shiftR regA co, regC), (instructions, nextPointer, output), False)
  -- cdv
  | opc == 7 = ((regA, regB, shiftR regA co), (instructions, nextPointer, output), False)
  where
    nextPointer = pointer + 2
    np = normalizePointer pointer
    (opc, lo) = instructions !! np
    co = combo lo regs

reverseSolve :: Registers -> Program -> Int
reverseSolve (_, regB, regC) prog@(instructions, _, _) = minimum $ foldl go [0] targets
  where
    targets = reverse $ expandInstructions instructions
    go starts target = do
      start <- starts
      n <- [0 .. 7]
      let regA = start * 8 + n
      let output = run (regA, regB, regC) prog
      guard (head output == target)
      return regA

part1 :: Registers -> Program -> [Int]
part1 = run

part2 :: Registers -> Program -> Int
part2 = reverseSolve

main :: IO ()
main = do
  s <- getContents
  let (input1, input2) = splitByEmpty s
  let regs = parseRegisters input1
  let prog = parseProgram input2
  printf "Part 1: %s\n" $ show (part1 regs prog)
  printf "Part 2: %d\n" $ part2 regs prog
