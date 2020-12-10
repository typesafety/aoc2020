module Solutions.Day10
    ( solve1
    , solve2
    ) where

import Data.List (maximum)
import Data.Sequence (Seq (..))

import qualified Data.IntMap.Strict as IM
import qualified Data.Sequence as Seq
import qualified Relude.Unsafe as Unsafe


type JoltCounters = IM.IntMap Int

solve1 :: Text -> Int
solve1 =
    product
    . flip map [(IM.! 1), (IM.! 3)]
    . (&)
    . flip execState initMap
    . getDiffs
    . sort
    . pJolts
  where
    initMap :: JoltCounters
    initMap = IM.fromList $ zip [0, 1, 2, 3] (repeat 0)

solve2 :: Text -> Int
solve2 =
    product
    . map (length . arrangements . toList)
    . islands 3
    . Seq.sort
    . pJoltsSeq

getDiffs :: [Int] -> State JoltCounters ()
getDiffs = go 0
  where
    go :: Int -> [Int] -> State JoltCounters ()
    go _ []             = incr 3
    go lastVal (x : xs) = incr (x - lastVal) >> go x xs

    incr :: Int -> State JoltCounters ()
    incr joltDiff = modify' $ IM.adjust (+ 1) joltDiff

islands :: forall a . (Ord a, Num a) => a -> Seq a -> [Seq a]
islands gap ss = go Empty ((0 Seq.<| Seq.sort ss) Seq.|> maximum ss + 3)
  where
    go :: Seq a -> Seq a -> [Seq a]
    go _ Empty = []
    go Empty (x :<| xs) = go (one x) xs
    go acc@(_ :|> y) rest@(x :<| xs)
        | x - y < gap = go (acc :|> x) xs
        | otherwise   = acc : go Empty rest

arrangements :: [Int] -> [[Int]]
arrangements xs@(_ : _ : _) =
    filter isValidSubSeq . map glue . subsequences . Unsafe.tail . Unsafe.init $ xs
  where
    glue :: [Int] -> [Int]
    glue ys = Unsafe.head xs : ys ++ [Unsafe.last xs]

    isValidSubSeq :: [Int] -> Bool
    isValidSubSeq []  = True
    isValidSubSeq [_] = True
    isValidSubSeq (y1 : y2 : ys) = y2 - y1 <= 3 && isValidSubSeq (y2 : ys)
arrangements [] = []
arrangements xs = [xs]

pJoltsSeq :: Text -> Seq Int
pJoltsSeq = fromList . pJolts

pJolts :: Text -> [Int]
pJolts = fromMaybe (error "⚡ Parse error ⚡") . mapM (readMaybe . toString) . lines
