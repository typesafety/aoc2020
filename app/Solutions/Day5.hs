module Solutions.Day5
    ( solveIO
    , solve1
    , solve2
    ) where

import Data.Foldable (maximum)
import Relude.Extra (next)
import Text.Read (read)
import Text.ParserCombinators.ReadP ( ReadP, munch, readP_to_S )

import qualified Misc.Misc as Misc (Part (..), solveIO)


solveIO :: Misc.Part -> FilePath -> IO ()
solveIO = Misc.solveIO (solve1, solve2)

solve1 :: Text -> Int
solve1 =
    maximum
    . mapMaybe (\ l -> idOfSeat <$> (evalSeatCode =<< pSeatCode l))
    . lines

solve2 :: Text -> Int
solve2 =
    fromMaybe (-1)
    . findMyId
    . sort
    . mapMaybe (\ l -> idOfSeat <$> (evalSeatCode =<< pSeatCode l))
    . lines
  where
    findMyId :: [Int] -> Maybe Int
    findMyId (x : y : xs)
        | y - x == 2 = Just $ x + 1
        | otherwise = findMyId (y : xs)
    findMyId _ = Nothing

idOfSeat :: Seat -> Int
idOfSeat seat = seatRow seat * 8 + seatCol seat

-- | Function from a span of numbers to a smaller span of numbers.
type Shrinker = (Int, Int) -> (Int, Int)

data Seat = Seat
    { seatRow :: Int
    , seatCol :: Int
    } deriving (Show, Ord, Eq)

data SeatCode = SeatCode [RowC] [ColC]
    deriving (Show)

data RowC = F | B
    deriving (Show, Read)

data ColC = L | R
    deriving (Show, Read)

-- * Calculating seat information

evalSeatCode :: SeatCode -> Maybe Seat
evalSeatCode (SeatCode row col) = Seat <$> evalRow row <*> evalCol col
  where
    evalRow :: [RowC] -> Maybe Int
    evalRow = binarySearch (0, 127) match
      where
        match :: RowC -> Shrinker
        match = \case
            F -> lower
            B -> upper

    evalCol :: [ColC] -> Maybe Int
    evalCol = binarySearch (0, 7) match
      where
        match :: ColC -> Shrinker
        match = \case
            L -> lower
            R -> upper

    lower :: Shrinker
    lower (low, high) = (low, (low + high) `div` 2)

    upper :: Shrinker
    upper (low, high) = ((low + high) `div` 2, high)

binarySearch :: forall a . (Int, Int) -> (a -> Shrinker) -> [a] -> Maybe Int
binarySearch startSpan match instructions =
    let (low, high) = shrink instructions startSpan
    in guarded (\ v -> v == next low || high == 0) high
  where
    shrink :: [a] -> (Int, Int) -> (Int, Int)
    shrink [] span       = span
    shrink (x : xs) span = shrink xs $ match x span

-- * Parser for boarding passes.

pSeatCode :: Text -> Maybe SeatCode
pSeatCode = fmap fst . listToMaybe . readP_to_S seatCodeP . toString
  where
    seatCodeP :: ReadP SeatCode
    seatCodeP = SeatCode <$> rowCodeP <*> colCodeP
      where
        colCodeP :: ReadP [ColC]
        colCodeP = map (read . pure) <$> munch (`elem` ['L', 'R'])

        rowCodeP :: ReadP [RowC]
        rowCodeP = map (read . pure) <$> munch (`elem` ['F', 'B'])
