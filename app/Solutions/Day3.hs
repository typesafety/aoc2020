module Solutions.Day3
    ( solveIO
    , solve1
    , solve2
    ) where

import qualified Control.Monad.Writer.Strict as W
import qualified Data.Sequence as S

import Misc.Misc (Part (..))


solveIO :: Part -> FilePath -> IO ()
solveIO part fp = readFileText fp >>= print . solver
  where
    solver :: (Text -> Int)
    solver = case part of
        P1 -> solve1
        P2 -> solve2

solve1 :: Text -> Int
solve1 = solve (Step (3, 1))

solve2 :: Text -> Int
solve2 input = product $ map (`solve` input) stepSizes
  where
    stepSizes :: [Step]
    stepSizes = map Step [(1, 1), (3, 1), (5, 1), (7, 1), (1, 2)]

solve :: Step -> Text -> Int
solve step = flip count step . parseGrid

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
newtype Step = Step (Int, Int)
newtype Point = Point
    { unPoint :: (Int, Int)
    }

data Obj = Open | Tree
    deriving (Show, Eq)

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

parseGrid :: Text -> TreeGrid
parseGrid = fromList . map pRow . lines
  where
    pRow :: Text -> Seq Obj
    pRow = fromList . map readObj . toString

