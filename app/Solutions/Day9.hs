module Solutions.Day9
    ( solve1
    , solve2
    ) where

import Data.Sequence (Seq (..))
import Relude.Extra (dup)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Sequence as Seq
import qualified Data.Set as Set

import qualified Misc.Misc as Misc


solve1 :: Text -> Int
solve1 txt = firstInvalid rest preamble initSums
  where
    (preamble, rest) :: (Seq Int, Seq Int) = Seq.splitAt 25 . pIntSeq $ txt

    initSums :: Sums
    initSums = pSums preamble

solve2 :: Text -> Int
solve2 = uncurry findWeakness . bimap solve1 pIntSeq . dup

-- | "Sums" is a map from sums to possible combinations of terms.
type Sums = IntMap (Set (Int, Int))
type Queue = Seq Int

insertSum :: Int -> Int -> Sums -> Sums
insertSum x y = IM.insertWith Set.union (x + y) (one (x, y))

moveQueue :: Int -> Queue -> (Int, Queue)
moveQueue _ Empty        = error "moveQueue: empty queue"
moveQueue new (x :<| xs) = (x, xs :|> new)

findWeakness :: Int -> Seq Int -> Int
findWeakness _ Empty = error "NO WEAKNESS FOUND"
findWeakness target nns@(_ :<| ns) =
    maybe (findWeakness target ns) (sumEnds . Seq.unstableSort) $ tryRange target nns
  where
    sumEnds :: Seq Int -> Int
    sumEnds xs = case (Seq.viewl xs, Seq.viewr xs) of
        (lower Seq.:< _, _ Seq.:> upper) -> lower + upper
        _ -> findWeakness target ns

-- Could be a lot faster if already-tried sums were stored somehow.
tryRange :: Int -> Seq Int -> Maybe (Seq Int)
tryRange target = go 0
  where
    go :: Int -> Seq Int -> Maybe (Seq Int)
    go _ Empty = Nothing
    go acc (n :<| ns)
        | acc < target = (n :<|) <$> go (acc + n) ns
        | acc > target = Nothing
        | otherwise    = Just Empty

firstInvalid :: Seq Int -> Queue -> Sums -> Int
firstInvalid Empty _ _         = error "firstInvalid: no invalid number found"
firstInvalid (n :<| ns) q sums
    | n `IS.notMember` IM.keysSet sums = n
    | otherwise = uncurry (firstInvalid ns) $ update n q sums

update :: Int -> Queue -> Sums -> (Queue, Sums)
update new q s = (newQueue, insertNewSums . trimSums $ s)
  where
    (toDrop, newQueue) :: (Int, Queue) = moveQueue new q

    insertNewSums :: Sums -> Sums
    insertNewSums = flip (flipfoldl' (insertSum new)) newQueue

    trimSums :: Sums -> Sums
    trimSums =
        IM.filter (not . null)
        . IM.map (Set.filter (\ (x, y) -> toDrop `notElem` [x, y]))

pIntSeq :: Text -> Seq Int
pIntSeq = fromList . map Misc.unsafeReadText . lines

pSums :: Seq Int -> Sums
pSums = flip go IM.empty
  where
    go :: Seq Int -> Sums -> Sums
    go Empty s      = s
    go (x :<| xs) s = go xs $ sums x xs s

    sums :: Int -> Seq Int -> Sums -> Sums
    sums _ Empty s      = s
    sums x (y :<| ys) s = sums x ys $ insertSum x y s
