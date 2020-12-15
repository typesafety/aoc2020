module Solutions.Day12
    ( solve1
    , solve2
    ) where

import Relude.Extra (bimapBoth)

import qualified Data.Text as T
import qualified Relude.Unsafe as Unsafe

import qualified Misc.Misc as Misc


newtype Angle = Angle Int
    deriving (Show)

signAngle :: TurnDir -> Angle -> Angle
signAngle tDir (Angle n) = case tDir of
    L -> Angle n
    R -> Angle (-n)

newtype Distance = Distance Int
    deriving (Show)

data CardinalDir = N | E | S | W
    deriving (Show)

toDegrees :: CardinalDir -> Int
toDegrees = \case
    N -> 90
    E -> 0
    S -> 270
    W -> 180

toCardinalDir :: Int -> CardinalDir
toCardinalDir = \case
    90  -> N
    0   -> E
    270 -> S
    180 -> W
    n   -> error $ "toCardinalDir: " <> show n <> " is not a valid angle"

data Dir
    = DirCardinal CardinalDir
    | DirForward
    deriving (Show)

data TurnDir = L | R
    deriving (Show)

data Instr
    = Move Dir Distance
    | Turn TurnDir Angle
    deriving (Show)

type Point = (Int, Int)
type Waypoint = Point

solve1 :: Text -> Int
solve1 =
    uncurry (+)
    . bimapBoth abs
    . flip evalState E
    . foldlM (flip step) (0, 0)
    . map pAction
    . lines

solve2 :: Text -> Int
solve2 =
    uncurry (+)
    . bimapBoth abs
    . flip evalState (10, 1)
    . foldlM (flip step2) (0, 0)
    . map pAction
    . lines

step2 :: Instr -> Point -> State Waypoint Point
step2 (Turn tDir angle) p = modify' (rotate (signAngle tDir angle)) >> pure p
step2 (Move dir (Distance n)) p = case dir of
    DirCardinal cDir -> modify' (mv cDir n) >> pure p
    DirForward -> foldl' (\ (wX, wY) -> bimap (+ wX) (+ wY)) p . replicate n <$> get

{- | Rotate the given waypoint around the origin (0, 0) by the given amount,
in steps of 90 degrees.
-}
rotate :: Angle -> Waypoint -> Waypoint
rotate (Angle amt) = Misc.applyN numSteps oneStep
  where
    numSteps :: Int
    numSteps = abs amt `mod` 360 `div` 90

    oneStep :: Waypoint -> Waypoint
    oneStep (wX, wY) = if amt > 0 then (-wY, wX) else (wY, -wX)

step :: Instr -> Point -> State CardinalDir Point
step (Turn tDir angle) p = modify' (turn (signAngle tDir angle)) >> pure p
step (Move dir (Distance n)) p = case dir of
    DirCardinal cDir -> pure $ mv cDir n p
    DirForward       -> (\ cDir -> mv cDir n p) <$> get

{- | Calculate a new direction given a starting direction and the amount to
turn in degrees (in steps of 90 degrees).
-}
turn :: Angle -> CardinalDir -> CardinalDir
turn (Angle amt) cd = toCardinalDir $ (toDegrees cd + amt) `mod` 360

-- | Move a point @n@ units in the given direction.
mv :: CardinalDir -> Int -> Point -> Point
mv direction amt (oldX, oldY) = case direction of
    N -> (oldX, oldY + amt)
    E -> (oldX + amt, oldY)
    S -> (oldX, oldY - amt)
    W -> (oldX - amt, oldY)

-- | Parse a text string into an instruction.
pAction :: Text -> Instr
pAction t =
    let (x, nn) :: (Text, Int) = second (Unsafe.read . toString) $ T.splitAt 1 t
    in either (buildMove nn) (buildTurn nn) (pDirType x)
  where
    buildMove :: Int -> Dir -> Instr
    buildMove n d = Move d (Distance n)

    buildTurn :: Int -> TurnDir -> Instr
    buildTurn n d = Turn d (Angle n)

    pDirType :: Text -> Either Dir TurnDir
    pDirType = \case
        "N" -> Left $ DirCardinal N
        "E" -> Left $ DirCardinal E
        "S" -> Left $ DirCardinal S
        "W" -> Left $ DirCardinal W
        "F" -> Left DirForward
        "L" -> Right L
        "R" -> Right R
        _   -> error "pAction: unexpected direction"
