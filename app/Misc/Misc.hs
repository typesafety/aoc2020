module Misc.Misc
    ( Part (..)
    , solveIO
    ) where


data Part = P1 | P2

{- | Utility for common task of reading text, applying function for the
correct part of the problem, and printing. -}
solveIO ::
    forall a b .
    (Show a, Show b) =>
       (Text -> a, Text -> b)
    -> Part
    -> FilePath
    -> IO ()
solveIO (solver1, solver2) part fp =
    readFileText fp >>= \ input -> either (print . ($ input)) (print . ($ input)) solver
  where
    solver :: Either (Text -> a) (Text -> b)
    solver = case part of
        P1 -> Left solver1
        P2 -> Right solver2
