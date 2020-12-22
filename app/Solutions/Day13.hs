{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Solutions.Day13
    ( solve1
    , solve2
    ) where

import qualified Data.Foldable as Foldable
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Misc.Misc as Misc


solve1 :: Text -> Int
solve1 txt = answer . Foldable.minimum $ earliestDepartures minStamp busIds
  where
    (minStamp, busIds) :: (Stamp, [BusId]) = pInput txt

    answer :: Departure -> Int
    answer = uncurry (*) . bimap unBusId (unStamp . subtract minStamp) . unDeparture

solve2 :: Text -> Int
solve2 = error "unsolved"

newtype Departure = Departure
    { unDeparture :: (BusId, Stamp)
    } deriving (Show, Eq)

instance Ord Departure where
    Departure (_, stamp1) <= Departure (_, stamp2) = stamp1 <= stamp2

newtype Stamp = Stamp
    { unStamp :: Int
    } deriving (Show, Eq, Ord, Num)

newtype BusId = BusId
    { unBusId :: Int
    } deriving (Show, Eq, Ord, Num, Hashable)

earliestDepartures :: Stamp -> [BusId] -> Seq Departure
earliestDepartures stamp =
    foldl' (\ ds bid -> Departure (bid, departure stamp bid) Seq.<| ds) Seq.empty

{- | Calculate the first possible departure on the given bus, with the given
time stamp as the earliest possible time.
-}
departure :: Stamp -> BusId -> Stamp
departure (Stamp minStamp) (BusId bId) = Stamp $ (minStamp `div` bId) * bId + bId

pInput :: Text -> (Stamp, [BusId])
pInput txt = case lines txt of
    [a, b] -> (pStamp a, pBusIds b)
    _ -> error "pInput: bad parse"
  where
    pStamp :: Text -> Stamp
    pStamp = Stamp . Misc.unsafeReadText

    pBusIds :: Text -> [BusId]
    pBusIds = map BusId . mapMaybe (readMaybe . toString) . T.splitOn ","
