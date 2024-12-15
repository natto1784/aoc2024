module Main where

import qualified Data.Map as M

type Coord = (Int, Int)

type Grid = M.Map Coord Char

data Dir = East | West | North | South deriving (Eq)

next :: Coord -> Dir -> Coord
next (x, y) East = (x + 1, y)
next (x, y) West = (x - 1, y)
next (x, y) North = (x, y - 1)
next (x, y) South = (x, y + 1)

dir :: Char -> Dir
dir '>' = East
dir '<' = West
dir '^' = North
dir 'v' = South

widen :: Char -> [Char]
widen '#' = "##"
widen 'O' = "[]"
widen '.' = ".."
widen '@' = "@."

parse :: [[Char]] -> (Coord, Grid)
parse raw =
  let [bot] = [(x, y) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row, ch == '@']
      raw' = [((x, y), ch) | (y, row) <- zip [0 ..] raw, (x, ch) <- zip [0 ..] row]
      grid = M.insert bot '.' $ M.fromList raw'
   in (bot, grid)

move :: [Dir] -> Coord -> Grid -> Grid
move [] _ grid = grid
move (d : ds) c grid = case move' c d grid of
  Nothing -> move ds c grid
  Just grid' -> move ds (next c d) grid'
  where
    move' :: Coord -> Dir -> Grid -> Maybe Grid
    move' c d grid = case M.lookup n grid of
      Just '.' -> Just grid
      Just '#' -> Nothing
      Just 'O' -> push n d <$> move' n d grid
      Just '[' | d `elem` [East, West] -> push n d <$> move' n d grid
      Just '[' -> push n d . push r d <$> (move' n d grid >>= move' r d)
      Just ']' | d `elem` [East, West] -> push n d <$> move' n d grid
      Just ']' -> push n d . push l d <$> (move' n d grid >>= move' l d)
      _ -> error "what?"
      where
        n :: Coord
        n@(x, y) = next c d
        r = (x + 1, y)
        l = (x - 1, y)

        push :: Coord -> Dir -> Grid -> Grid
        push c d grid = case M.lookup c grid of
          Just ch -> M.insert (next c d) ch $ M.insert c '.' grid
          Nothing -> error "what?"

gps :: Grid -> Char -> Int
gps grid char = sum [y * 100 + x | ((x, y), ch) <- M.assocs grid, ch == char]

main :: IO ()
main =
  do
    (raw, map dir . concat -> moves) <- break null . lines <$> readFile "./inputs/day15.in"

    let (bot1, grid1) = parse raw
        (bot2, grid2) = parse . map (concatMap widen) $ raw

    putStr "Part 1: " >> print (gps (move moves bot1 grid1) 'O')
    putStr "Part 2: " >> print (gps (move moves bot2 grid2) '[')
