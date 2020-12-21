module Solutions.Day11
    ( solve1
    , solve2
    ) where

import Data.Matrix (Matrix (..))

import qualified Data.Matrix as Mtx


solve1 :: Text -> Int
solve1 = length . filter (== Seat Occupied) . Mtx.toList . run step1 . pMatrix

solve2 :: Text -> Int
solve2 = length . filter (== Seat Occupied) . Mtx.toList . run step2 . pMatrix

data Pos = Floor | Seat SeatStatus
    deriving (Show, Read, Eq)
data SeatStatus = Occupied | Vacant
    deriving (Show, Read, Eq)

type Point = (Int, Int)

run :: (Matrix Pos -> Matrix Pos) -> Matrix Pos -> Matrix Pos
run stepFun m =
    let newMatrix :: Matrix Pos = stepFun m
    in if newMatrix == m then m else run stepFun newMatrix

step2 :: Matrix Pos -> Matrix Pos
step2 mtx = Mtx.mapPos change mtx
  where
    change :: Point -> Pos -> Pos
    change pt pos = case mtx Mtx.! pt of
        Seat Occupied
            | length (filter (== Occupied) (visibleSeats pt mtx)) >= 5
                -> Seat Vacant
        Seat Vacant
            | (Occupied `notElem`) $ visibleSeats pt mtx
                -> Seat Occupied
        _ -> pos

    visibleSeats :: Point -> Matrix Pos -> [SeatStatus]
    visibleSeats (r, c) m =
        mapMaybe findSeat [ptsE, ptsN, ptsW, ptsS, ptsNE, ptsSE, ptsSW, ptsNW]
      where
        findSeat :: [Point] -> Maybe SeatStatus
        findSeat [] = Nothing
        findSeat (p : ps) = uncurry Mtx.safeGet p m >>= \case
            Floor   -> findSeat ps
            Seat ss -> Just ss

        ptsE, ptsW, ptsN, ptsS, ptsNE, ptsSE, ptsSW, ptsNW :: [Point]
        ptsE  = [(r, x) | x <- [c + 1        ..]]
        ptsW  = [(r, x) | x <- [c - 1, c - 2 ..]]
        ptsS  = [(y, c) | y <- [r + 1        ..]]
        ptsN  = [(y, c) | y <- [r - 1, r - 2 ..]]
        ptsNE = zipWith (curry (bimap fst snd)) ptsN ptsE
        ptsSE = zipWith (curry (bimap fst snd)) ptsS ptsE
        ptsSW = zipWith (curry (bimap fst snd)) ptsS ptsW
        ptsNW = zipWith (curry (bimap fst snd)) ptsN ptsW

step1 :: Matrix Pos -> Matrix Pos
step1 m = Mtx.mapPos change m
  where
    (width, height) :: Point = (ncols m, nrows m)

    change :: Point -> Pos -> Pos
    change p pos = case m Mtx.! p of
        Seat Occupied | crowdedEnough (map (m Mtx.!) $ adjacent p) -> Seat Vacant
        Seat Vacant   | emptyEnough (map (m Mtx.!) $ adjacent p)   -> Seat Occupied
        _ -> pos

    -- Return the 8 points adjacent to the input point.
    adjacent :: Point -> [Point]
    adjacent origin@(x, y) =
        filter (/= origin)
        . filter isInBounds
        $ [(a, b) | a <- [x - 1 .. x + 1], b <- [y - 1 .. y + 1]]

    emptyEnough :: [Pos] -> Bool
    emptyEnough = (Seat Occupied `notElem`)

    crowdedEnough :: [Pos] -> Bool
    crowdedEnough = (>= 4) . length . filter (== Seat Occupied)

    isInBounds :: Point -> Bool
    isInBounds (r, c) = and [r <= height, r > 0, c <= width, c > 0]

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
