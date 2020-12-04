module Solutions.Day3 where

import qualified Control.Monad.Writer.Strict as W
import qualified Data.Sequence as S

import Misc.Misc (Part (..))


solveIO :: Part -> FilePath -> IO ()
solveIO part fp = case part of
    P1 -> readFileText fp >>= print . solve
    P2 -> putTextLn "Not yet solved!"

solve :: Text -> Int
solve = flip count (Step (3, 1)) . parseGrid

count :: TreeGrid -> Step -> Int
count grid (Step (dx, dy)) =
    length . filter (== Tree) . W.execWriter . go grid $ Point (0, 0)
  where
    go :: TreeGrid -> Point -> W.Writer [Obj] ()
    go tg currPos@(Point (_, currY)) =
        if currY >= height
            then pass
            else W.tell [objAt currPos] >> go tg (move currPos)
      where
        move :: Point -> Point
        move = Point . bimap ((`mod` width) . (+ dx)) (+ dy) . unPoint

        -- Get object at the given point, defaulting to Open if outside the grid.
        objAt :: Point -> Obj
        objAt (Point (x, y)) = fromMaybe Open (S.lookup y tg >>= S.lookup x)

    height :: Int
    height = length grid

    width :: Int
    width = case S.viewl grid of
        S.EmptyL      -> 0
        topRow S.:< _ -> length topRow

{- | Horizontal movement on the left, vertical on the right. Positive values
move to the right and down.
-}
newtype Step = Step
    { unStep :: (Int, Int)
    }
newtype Point = Point
    { unPoint :: (Int, Int)
    }

data Obj = Open | Tree
    deriving (Show, Eq)

showObj :: Obj -> Char
showObj = \case
    Open -> '.'
    Tree -> '#'

-- | Read an object from a Char, crash on invalid character.
readObj :: Char -> Obj
readObj = \case
    '.' -> Open
    '#' -> Tree
    _   -> error "readObj: invalid character"

{- | The map is a nested sequence. The inner sequence represents a row
row in on the map -}
type TreeRow = Seq Obj
type TreeGrid = Seq TreeRow

showGrid :: TreeGrid -> Text
showGrid = unlines . toList . fmap showRow
  where
    showRow :: TreeRow -> Text
    showRow = fromString . toList . fmap showObj

parseGrid :: Text -> TreeGrid
parseGrid = fromList . map pRow . lines
  where
    pRow :: Text -> Seq Obj
    pRow = fromList . map readObj . toString

