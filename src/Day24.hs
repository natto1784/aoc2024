module Main where

import qualified AoC as A
import Control.Arrow (Arrow (second))
import Data.Bits (shiftL, xor, (.|.))
import Data.Char (digitToInt)
import Data.Either (rights)
import Data.List (foldl', mapAccumL)
import qualified Data.Map as M
import Text.Parsec (char, digit, letter, many1, newline, parse, sepEndBy1, string, try, (<|>))
import Text.Parsec.String (Parser)

type Gate = Bool -> Bool -> Bool

type Connection = (String, Gate, String)

type States = M.Map String (Either Connection Bool)

parseInitial :: Parser (String, Bool)
parseInitial =
  (,)
    <$> many1 (letter <|> digit)
    <*> (string ": " *> (toEnum . digitToInt <$> digit))

parseConnection :: Parser (String, (String, Gate, String))
parseConnection = do
  a <- many1 (letter <|> digit)
  char ' '
  op <-
    (try (string "AND") >> return (&&))
      <|> (try (string "OR") >> return (||))
      <|> (try (string "XOR") >> return xor)
  char ' '
  b <- many1 (letter <|> digit)
  string " -> "
  c <- many1 (letter <|> digit)
  return (c, (a, op, b))

parseInput :: Parser States
parseInput =
  do
    initial <- parseInitial `sepEndBy1` newline
    let m = M.fromList (map (second Right) initial)
    newline
    connections <- parseConnection `sepEndBy1` newline
    return $ foldr (\(a, b) -> M.insert a (Left b)) m connections

simulate :: States -> String -> (States, Bool)
simulate s line = case s M.! line of
  Right bit -> (s, bit)
  Left (lineA, op, lineB) ->
    let (s', a) = simulate s lineA
        (s'', b) = simulate s' lineB
        value = a `op` b
     in (M.insert line (Right value) s'', value)

simulateAll :: States -> States
simulateAll states = fst . mapAccumL simulate states $ M.keys states

wires :: Char -> States -> [Bool]
wires ch = rights . map snd . filter ((== ch) . head . fst) . M.toDescList

binToDec :: [Bool] -> Int
binToDec = foldl' (\num b -> num `shiftL` 1 .|. fromEnum b) 0

part1 :: States -> Int
part1 = binToDec . wires 'z' . simulateAll

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day24.in"
    let states = A.extract $ parse parseInput "" raw

    putStr "Part 1: " >> print (part1 states)
    -- TODO: write code
    putStrLn "Part 2: Done by hand :')"
