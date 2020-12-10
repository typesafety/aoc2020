module Solutions.Day10 where

import qualified Data.IntMap.Strict as IM


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
solve2 = error "Not yet solved"


getDiffs :: [Int] -> State JoltCounters ()
getDiffs = go 0
  where
    go :: Int -> [Int] -> State JoltCounters ()
    go _ []             = incr 3
    go lastVal (x : xs) = incr (x - lastVal) >> go x xs

    incr :: Int -> State JoltCounters ()
    incr joltDiff = modify' $ IM.adjust (+ 1) joltDiff


pJolts :: Text -> [Int]
pJolts = fromMaybe (error "⚡ Parse error ⚡") . mapM (readMaybe . toString) . lines
