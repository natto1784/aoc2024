module Main where

import qualified AoC as A (extract)
import Text.Parsec (char, digit, many1, newline, parse, sepBy1, sepEndBy1, string)
import Text.Parsec.String (Parser)

type Equation = (Int, [Int])

type Operator = Int -> Int -> Int

parseEqns :: Parser [Equation]
parseEqns = parseEqn `sepEndBy1` newline
  where
    parseEqn :: Parser Equation
    parseEqn =
      (,)
        <$> (read <$> many1 digit <* string ": ")
        <*> (read <$> many1 digit) `sepBy1` char ' '

-- concatenate integers
cc :: Operator
cc a b = a * (10 ^ length b) + b
  where
    -- this is faster than log_10
    length :: Int -> Int
    length 0 = 0
    length x = 1 + length (x `div` 10)

check :: [Operator] -> Equation -> Bool
check operators (target, operand : operands) = go operand operands
  where
    go :: Int -> [Int] -> Bool
    go i [] = i == target
    go i (x : xs)
      | i > target = False -- prune
      | otherwise = any (\op -> go (i `op` x) xs) operators

getSum :: [Operator] -> [Equation] -> Int
getSum ops = sum . map fst . filter (check ops)

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day7.in"
    let equations = A.extract $ parse parseEqns "" raw

    putStr "Part 1: " >> print (getSum [(+), (*)] equations)
    putStr "Part 2: " >> print (getSum [(+), (*), cc] equations)
