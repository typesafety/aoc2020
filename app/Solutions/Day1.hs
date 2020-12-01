module Solutions.Day1
    ( solveIO
    , solve
    ) where

import Data.Text.Read (decimal)


solve :: Text -> Maybe Integer
solve =
    fmap (\ (a, b, c) -> a * b * c)
    . findSum
    . rights
    . map (second fst . decimal)
    . words
  where
    findSum :: [Integer] -> Maybe (Integer, Integer, Integer)
    findSum ints = listToMaybe
        [ (a, b, c)
        | a <- ints
        , b <- ints
        , c <- ints
        , a + b + c == 2020
        ]

solveIO :: FilePath -> IO ()
solveIO fp =
    readFileText fp
    >>= flip whenJust print . solve  -- The flip is important to decrease readability
