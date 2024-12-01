module AoC where

import Text.Parsec

extract :: Either ParseError a -> a
extract (Left err) = error ("Parsing failed: " ++ show err)
extract (Right val) = val

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (x ==)
