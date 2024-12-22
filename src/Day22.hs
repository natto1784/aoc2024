module Main where

import Data.Bits (Bits (xor))
import qualified Data.Map as M

type Bananas = M.Map Int Int

evolve :: Int -> Int
evolve =
  (`mod` 16777216)
    . (xor <*> (* 2048))
    . (`mod` 16777216)
    . (xor <*> (`div` 32))
    . (`mod` 16777216)
    . (xor <*> (* 64))

part1 :: [Int] -> Int
part1 = sum . map ((!! 2000) . iterate evolve)

hash :: (Int, Int, Int, Int) -> Int
hash (a, b, c, d) = a + 19 * (b + 19 * (c + 19 * d))

bananas :: Bananas -> [Int] -> Bananas
bananas f (a : b : c : d : e : xs) =
  let key = hash (b - a, c - b, d - c, e - d)
      next = bananas f (b : c : d : e : xs)
   in case M.lookup key f of
        Nothing -> M.insert key e next
        Just _ -> next
bananas f _ = f

part2 :: [Int] -> Int
part2 =
  maximum
    . M.elems
    . M.unionsWith (+)
    . map
      (bananas M.empty . map (`mod` 10) . take 2001 . iterate evolve)

main :: IO ()
main =
  do
    buyers <- map read . words <$> readFile "./inputs/day22.in"

    putStr "Part 1: " >> print (part1 buyers)
    putStr "Part 2: " >> print (part2 buyers)
