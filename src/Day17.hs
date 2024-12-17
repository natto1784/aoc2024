module Main where

import qualified AoC as A (extract)
import Data.Bits (Bits (shiftR, xor))
import Data.List (isSuffixOf)
import Text.Parsec (char, digit, many1, newline, parse, sepBy1, string)
import Text.Parsec.String (Parser)

data Instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv deriving (Enum, Show)

type Program = [Int]

type PC = Int

type Registers = (Int, Int, Int)

type Output = [Int]

parseProg :: Parser Program
parseProg = string "Program: " *> ((read <$> many1 digit) `sepBy1` char ',')

parseRegs :: Parser Registers
parseRegs = do
  a <- string "Register A: " *> (read <$> many1 digit) <* newline
  b <- string "Register B: " *> (read <$> many1 digit) <* newline
  c <- string "Register C: " *> (read <$> many1 digit) <* newline
  return (a, b, c)

parseInfo :: Parser (Registers, Program)
parseInfo = (,) <$> (parseRegs <* newline) <*> parseProg

steps :: PC -> Registers -> Program -> Output
steps pc regs prog = step pc regs
  where
    step :: PC -> Registers -> Output
    step pc regs@(a, b, c) = case drop pc prog of
      [] -> []
      ins : op : _ ->
        case toEnum ins of
          Adv -> step (pc + 2) (a `shiftR` combo, b, c)
          Bxl -> step (pc + 2) (a, b `xor` op, c)
          Bst -> step (pc + 2) (a, combo `mod` 8, c)
          Jnz | a /= 0 -> step op regs
          Jnz -> step (pc + 2) regs
          Bxc -> step (pc + 2) (a, b `xor` c, c)
          Out -> combo `mod` 8 : step (pc + 2) regs
          Bdv -> step (pc + 2) (a, a `shiftR` combo, c)
          Cdv -> step (pc + 2) (a, b, a `shiftR` combo)
        where
          combo :: Int
          combo = case op of
            0 -> 0
            1 -> 1
            2 -> 2
            3 -> 3
            4 -> a
            5 -> b
            6 -> c
            _ -> error "invalid operand"

--------------------------------------------------------------
-- Will only work given the following conditions are true   --
-- 1. There is only a single loop at the end of program     --
-- 2. A is right shifted by 3 every loop                    --
-- 3. B is the value being printed                          --
-- 4. Based on the instructions, B is deterministic given A --
--------------------------------------------------------------
part2 :: Program -> Int
part2 prog = go 0
  where
    go :: Int -> Int
    go a
      | output == prog = a
      | output `isSuffixOf` prog = go $ a * 8
      | otherwise = go $ a + 1
      where
        output = steps 0 (a, 0, 0) prog

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day17.in"

    let (regs, prog) = A.extract $ parse parseInfo "" raw

    putStr "Part 1: " >> print (steps 0 regs prog)
    putStr "Part 2: " >> print (part2 prog)
