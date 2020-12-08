module Misc.Misc
    ( -- * Boilerplate for input and output
      Part (..)
    , solveIO

      -- * Other useful stuff
    , if'
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

-- | If-then-else as a function.
if' :: Bool -> a -> a -> a
if' b x y = if b then x else y
