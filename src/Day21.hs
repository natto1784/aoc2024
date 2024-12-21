module Main where

import Data.List (foldl', minimumBy, nub)
import qualified Data.Map as M
import Data.Ord (comparing)

-- TODO: ugloid and slow solution, please clean

type Vec = (Int, Int)

type Table = M.Map (Char, Char) [String]

type Memo = M.Map (Char, Char, Int) Int

coord :: Char -> Vec
-- keypad
coord '0' = (1, 3)
coord 'A' = (2, 3)
coord '1' = (0, 2)
coord '2' = (1, 2)
coord '3' = (2, 2)
coord '4' = (0, 1)
coord '5' = (1, 1)
coord '6' = (2, 1)
coord '7' = (0, 0)
coord '8' = (1, 0)
coord '9' = (2, 0)
-- direction
coord 'a' = (2, 0)
coord '^' = (1, 0)
coord '<' = (0, 1)
coord 'v' = (1, 1)
coord '>' = (2, 1)
coord _ = error "what?"

path :: Char -> Char -> [String]
path a b =
  let ((x1, y1), (x2, y2)) = (coord a, coord b)
      moves =
        replicate (abs (y2 - y1)) (if y2 > y1 then 'v' else '^')
          ++ replicate (abs (x2 - x1)) (if x2 > x1 then '>' else '<')
   in if
        | a == '<' || x1 == 0 && y2 == 3 -> [reverse moves ++ "a"]
        | b == '<' || y1 == 3 && x2 == 0 -> [moves ++ "a"]
        | otherwise -> nub [reverse moves ++ "a", moves ++ "a"]

seqc :: Table -> Int -> String -> Int
seqc table n = fst . seqc' M.empty (n + 1) 'A'
  where
    seqc' :: Memo -> Int -> Char -> String -> (Int, Memo)
    seqc' memo 0 _ s = (length s, memo)
    seqc' memo n start s = foldl' loop (0, memo) $ zip (start : s) s
      where
        loop :: (Int, Memo) -> (Char, Char) -> (Int, Memo)
        loop (total, memo) (a, b) = case (a, b, n) `M.lookup` memo of
          Just x -> (total + x, memo)
          Nothing ->
            let paths = table M.! (a, b)
                (x, memo') =
                  minimumBy
                    (comparing fst)
                    (map (seqc' memo (n - 1) 'a') paths)
             in (total + x, M.insert (a, b, n) x memo')

complexity :: Table -> Int -> String -> Int
complexity table n s = a * b
  where
    a = read $ init s
    b = seqc table n s

main :: IO ()
main =
  do
    input <- words <$> readFile "./inputs/day21.in"
    let keys = "A0123456789a<v>^"
        table = M.fromList [((a, b), path a b) | a <- keys, b <- keys]

    putStr "Part 1: " >> print (sum $ map (complexity table 2) input)
    putStr "Part 2: " >> print (sum $ map (complexity table 25) input)
