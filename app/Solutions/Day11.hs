module Solutions.Day11
    ( solve1
    , solve2
    ) where

import Data.Matrix (Matrix (..))

import qualified Data.Matrix as Mtx


solve1 :: Text -> Int
solve1 = length . filter (== Seat Occupied) . Mtx.toList . run . pMatrix

solve2 :: Text -> Int
solve2 = error "Not yet solved"

data Pos = Floor | Seat SeatStatus
    deriving (Show, Read, Eq)
data SeatStatus = Occupied | Vacant
    deriving (Show, Read, Eq)

type Point = (Int, Int)

run :: Matrix Pos -> Matrix Pos
run m =
    let newMatrix :: Matrix Pos = step m
    in if newMatrix == m then m else run newMatrix

step :: Matrix Pos -> Matrix Pos
step m = Mtx.mapPos change m
  where
    (width, height) :: Point = (ncols m, nrows m)

    change :: Point -> Pos -> Pos
    change p pos = case m Mtx.! p of
        Seat Occupied | crowdedEnough (map (m Mtx.!) $ adjacent p) -> Seat Vacant
        Seat Vacant   | emptyEnough (map (m Mtx.!) $ adjacent p)   -> Seat Occupied
        _ -> pos

    adjacent :: Point -> [Point]
    adjacent origin@(x, y) =
        filter isInBounds [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]
      where
        isInBounds :: Point -> Bool
        isInBounds (t, u) = and [t <= height, t > 0, u <= width, u > 0, origin /= (t, u)]

    emptyEnough :: [Pos] -> Bool
    emptyEnough = (Seat Occupied `notElem`)

    crowdedEnough :: [Pos] -> Bool
    crowdedEnough = (>= 4) . length . filter (== Seat Occupied)

pMatrix :: Text -> Matrix Pos
pMatrix = Mtx.fromLists . map pRow . lines
  where
    pRow :: Text -> [Pos]
    pRow = map pPos . toString
      where
        pPos :: Char -> Pos
        pPos = \case
            '.' -> Floor
            '#' -> Seat Occupied
            'L' -> Seat Vacant
            _   -> error "pPos: bad parse"
