module Solutions.Day17
    ( solve1
    , solve2
    ) where

import qualified Misc.Misc as Misc

import qualified Data.HashMap.Strict as HM
import qualified Data.HashSet as HS
import qualified Data.List as List
import qualified Data.Matrix as Mtx


solve1 :: Text -> Text
solve1 = const
    $ "Solution for part 1 is the same as for part 2, but with 3"
    <> " dimensions instead of 4."

solve2 :: Text -> Int
solve2 = HM.size . HM.filter (== Active) . Misc.applyN 6 runCycle . pInitState

data Cube = Active | Inactive
    deriving (Eq)

type PocketDim = HM.HashMap Point Cube

type Point = (Int, Int, Int, Int)

runCycle :: PocketDim -> PocketDim
runCycle pd = HM.mapWithKey (changeState newPd) newPd
  where
    newPd :: PocketDim
    newPd = addCloseCubes pd

addCloseCubes :: PocketDim -> PocketDim
addCloseCubes pd = insertInactives pd . HS.toList . allNeighbors $ pd
  where
    activePoints :: PocketDim -> HS.HashSet Point
    activePoints = HS.filter (\ p -> (pd HM.!? p) == Just Active) . HM.keysSet

    allNeighbors :: PocketDim -> HS.HashSet Point
    allNeighbors =
        HS.foldr HS.union HS.empty
        . HS.map (HS.fromList . neighbors)
        . activePoints

    insertInactives :: PocketDim -> [Point] -> PocketDim
    insertInactives = foldl' (\ pockD p -> HM.insertWith arg2 p Inactive pockD)
      where
        arg2 :: a -> a -> a
        arg2 _ b = b

changeState :: PocketDim -> Point -> Cube -> Cube
changeState pd p c =
    case c of
        Active
            | length activeNeighbors `elem` [2, 3] -> c
            | otherwise -> Inactive
        Inactive
            | length activeNeighbors == 3 -> Active
            | otherwise -> c
  where
    activeNeighbors :: [Cube]
    activeNeighbors =
        filter (== Active)
        . map (flip (HM.findWithDefault Inactive) pd)
        . neighbors
        $ p

neighbors :: Point -> [Point]
neighbors origin@(x, y, z, t) = List.delete origin $
    [ (a, b, c, d)
    | a <- [x - 1 .. x + 1]
    , b <- [y - 1 .. y + 1]
    , c <- [z - 1 .. z + 1]
    , d <- [t - 1 .. t + 1]
    ]

pInitState :: Text -> PocketDim
pInitState =
    fromList
    . Mtx.toList
    . Mtx.mapPos (\ (x, y) c -> ((x, y, 0, 0), c))
    . Mtx.fromLists
    . map pRow
    . lines
  where
    pRow :: Text -> [Cube]
    pRow = map pCube . toString

    pCube :: Char -> Cube
    pCube = \case
        '.' -> Inactive
        '#' -> Active
        _   -> error "pCube: parse error"
