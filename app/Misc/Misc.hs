module Misc.Misc
    ( -- * Boilerplate for input and output
      Part (..)
    , solveIO

      -- * Other useful stuff
    , if'
    , applyN
    , applyNM

      -- * Particularly unsafe useful stuff
    , unsafeReadText
    ) where

import qualified Relude.Unsafe as Unsafe


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

-- | Repeatedly apply a function n times.
applyN :: Int -> (a -> a) -> a -> a
applyN n f x
    | n > 0  = applyN (n - 1) f (f x)
    | n == 0 = x
    | otherwise = error "applyN: negative integer"

applyNM :: Monad m => Int -> (a -> m a) -> a -> m a
applyNM n f x
    | n > 0     = f x >>= applyNM (n - 1) f
    | n == 0    = return x
    | otherwise = error "applyNM: negative integer"

-- | Like "Text.Read.read", but for Text.
unsafeReadText :: Read a => Text -> a
unsafeReadText = Unsafe.read . toString
