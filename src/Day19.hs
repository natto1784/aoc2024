module Main where

import qualified AoC as A
import Data.Array as A (listArray, (!))
import Data.List (tails)
import qualified Data.Map as M
import Text.Parsec (letter, many1, parse, sepBy1, string)
import Text.Parsec.String (Parser)

data Trie = Trie Bool !(M.Map Char Trie) deriving (Show)

parseTowels :: Parser [String]
parseTowels = many1 letter `sepBy1` string ", "

fromWord :: String -> Trie
fromWord = foldr (\x xs -> Trie False (M.singleton x xs)) (Trie True M.empty)

insertTrie :: String -> Trie -> Trie
insertTrie [] (Trie _ t) = Trie True t
insertTrie (x : xs) (Trie b t) =
  Trie b $ M.alter (Just . maybe (fromWord xs) (insertTrie xs)) x t

possible :: Trie -> String -> Int
possible trie ys = dp ! 0
  where
    l = length ys

    dp =
      A.listArray
        (0, l)
        [combos s i trie | (i, s) <- zip [0 .. l] $ tails ys]

    combos :: String -> Int -> Trie -> Int
    combos "" _ (Trie b _) = fromEnum b
    combos (y : ys) i (Trie b t) =
      maybe 0 (combos ys $ i + 1) (M.lookup y t)
        + if b then dp ! i else 0

main :: IO ()
main =
  do
    (rawTowels : _ : designs) <- lines <$> readFile "./inputs/day19.in"
    let towels = A.extract $ parse parseTowels "" rawTowels
        trie = foldr insertTrie (Trie False M.empty) towels
        combinations = map (possible trie) designs

    putStr "Part 1: " >> print (length . filter (> 0) $ combinations)
    putStr "Part 2: " >> print (sum combinations)
