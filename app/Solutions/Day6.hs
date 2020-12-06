module Solutions.Day6
    ( solve1
    , solve2
    ) where

import Data.List (foldl1, intersect)

import qualified Data.Text as T


solve1 :: Text -> Int
solve1 = sum . map (length . ordNub . toString . T.filter (/= '\n')) . T.splitOn "\n\n"

solve2 :: Text -> Int
solve2 = sum . map (length . foldl1 intersect . map toString . lines) . T.splitOn "\n\n"
