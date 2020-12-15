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

rotate :: Int -> Waypoint -> Waypoint
rotate amt oldWaypoint = newWaypoint
  where
    numSteps :: Int
    numSteps = abs amt `mod` 360 `div` 90

    newWaypoint :: Waypoint
    newWaypoint = Misc.applyN numSteps oneStep oldWaypoint

    oneStep :: Waypoint -> Waypoint
    oneStep (wX, wY) = if amt > 0 then (-wY, wX) else (wY, -wX)

step2 :: Instr -> Point -> State Waypoint Point
step2 (Turn tDir (Angle n)) p = modify' (rotate signedAngle) >> pure p
  where
    signedAngle :: Int
    signedAngle = case tDir of
        L -> n
        R -> (-n)
step2 (Move dir (Distance n)) p = case dir of
    DirCardinal cDir -> modify' (mv cDir n) >> pure p
    DirForward -> foldl' (\ (wX, wY) -> bimap (+ wX) (+ wY)) p . replicate n <$> get

step :: Instr -> Point -> State CardinalDir Point
step (Turn tDir (Angle n)) p = do
    let angle = case tDir of
            L -> n
            R -> (-n)
    modify' (turn angle)
    pure p
step (Move dir (Distance n)) p = case dir of
    DirCardinal cDir -> pure $ mv cDir n p
    DirForward       -> (\ cDir -> mv cDir n p) <$> get

turn :: Int -> CardinalDir -> CardinalDir
turn amt cd = toCardinalDir $ (toDegrees cd + amt) `mod` 360

mv :: CardinalDir -> Int -> Point -> Point
mv direction amt (oldX, oldY) = case direction of
    N -> (oldX, oldY + amt)
    E -> (oldX + amt, oldY)
    S -> (oldX, oldY - amt)
    W -> (oldX - amt, oldY)

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
