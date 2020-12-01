module Solutions.Day1
    ( solveIO
    , solve
    ) where

import Data.Text.Read (decimal)

solve :: Text -> Maybe Integer
solve =
    fmap (\ (a, b, c) -> a * b * c)
    . findSum
    . getRights
    . map (second fst . decimal)
    . words
  where
    getRights :: [Either a b] -> [b]
    getRights = \case
        []           -> []
        Left _ : xs  -> getRights xs
        Right b : xs -> b : getRights xs

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
