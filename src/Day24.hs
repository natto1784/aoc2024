module Main where

import qualified AoC as A
import Control.Arrow (Arrow (second))
import Data.Bits (shiftL, xor, (.|.))
import Data.Char (digitToInt)
import Data.Either (rights)
import Data.List (foldl', mapAccumL, sort)
import qualified Data.Map as M
import Text.Parsec (char, digit, letter, many1, newline, parse, sepEndBy1, string, try, (<|>))
import Text.Parsec.String (Parser)

data Gate = And | Or | Xor deriving (Eq, Enum)

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
    (try (string "AND") >> return And)
      <|> (try (string "OR") >> return Or)
      <|> (try (string "XOR") >> return Xor)
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
        operator = [(&&), (||), xor] !! fromEnum op
        value = a `operator` b
     in (M.insert line (Right value) s'', value)

simulateAll :: States -> States
simulateAll states = fst . mapAccumL simulate states $ M.keys states

wires :: Char -> States -> [Bool]
wires ch = rights . map snd . filter ((== ch) . head . fst) . M.toDescList

binToDec :: [Bool] -> Int
binToDec = foldl' (\num b -> num `shiftL` 1 .|. fromEnum b) 0

part1 :: States -> Int
part1 = binToDec . wires 'z' . simulateAll

fullAdder :: States -> String -> Maybe [String]
fullAdder states num =
  let z = 'z' : num
      x = 'x' : num
      y = 'y' : num
      Left (a, fn1, b) = states M.! z
      Left (_, fn2, _) = states M.! a
      Left (_, fn3, _) = states M.! b
      xor1 = rLookup (x, Xor, y)
      xor2 = rLookupP (xor1, Xor)
   in case fn1 of
        Xor -> case (fn2, fn3) of
          (And, Or) -> Just [a, xor1]
          (Or, And) -> Just [b, xor1]
          _ -> Nothing
        _ -> Just [z, xor2]
  where
    rLookup :: Connection -> String
    rLookup (a, op, b) =
      head
        [ s
          | (s, c) <- M.assocs states,
            c `elem` [Left (a, op, b), Left (b, op, a)]
        ]
    rLookupP :: (String, Gate) -> String
    rLookupP (a, g) =
      head
        [ s
          | (s, c) <- M.assocs states,
            Left (x, op, y) <- [c],
            (a, g) `elem` [(x, op), (y, op)]
        ]

part2 :: States -> [String]
part2 states =
  let zLength = length (wires 'x' states) + 1
   in sort
        [ line
          | x <- [1 .. zLength - 2],
            let num = if x < 10 then '0' : show x else show x,
            Just lines <- [fullAdder states num],
            line <- lines
        ]

main :: IO ()
main =
  do
    raw <- readFile "./inputs/day24.in"
    let states = A.extract $ parse parseInput "" raw

    putStr "Part 1: " >> print (part1 states)
    putStr "Part 2: " >> print (part2 states)
