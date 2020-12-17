module Solutions.Day15
    ( solve1
    , solve2
    ) where

import Relude.Extra (dup)

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

import qualified Misc.Misc as Misc


newtype Turn = Turn
    { unTurn :: Int
    } deriving (Show, Eq)

onTurn :: (Int -> Int) -> Turn -> Turn
onTurn f = Turn . f . unTurn

-- | Store what turn a number was spoken on last.
type History = IntMap Turn

{- | Set up the history with the starting numbers, and return the 'last number spoken',
and the upcoming turn.
-}
start :: NonEmpty Int -> State History (Int, Turn)
start startingNums =
    let (initNums, lastNum) :: ([Int], Int) = bimap init last $ dup startingNums
    in (lastNum, ) <$> initHistory initNums
  where
    initHistory :: [Int] -> State History Turn
    initHistory = go (Turn 1)
      where
        go :: Turn -> [Int] -> State History Turn
        go currTurn []       = pure currTurn
        go currTurn (n : ns) = do
            modify' (IM.insert n currTurn)
            go (onTurn (+ 1) currTurn) ns

speak :: Int -> Turn -> State History (Int, Turn)
speak lastNum currTurn = do
    diff <- gets (IM.lookup lastNum) >>= \case
        Nothing              -> pure 0
        Just (Turn lastSeen) -> pure $ unTurn currTurn - lastSeen
    modify' (IM.insert lastNum currTurn)
    pure (diff, onTurn (+ 1) currTurn)

-- | Play until the specified turn.
playUntil :: Turn -> NonEmpty Int -> (Int, History)
playUntil untilTurn startingNums = usingState IM.empty $ do
    (lastNum, currTurn) <- start startingNums
    play lastNum currTurn
  where
    play :: Int -> Turn -> State History Int
    play n t
        | t == untilTurn = pure n
        | otherwise = speak n t >>= uncurry play

pStartingNums :: Text -> NonEmpty Int
pStartingNums = fromList . map Misc.unsafeReadText . T.splitOn ","

solve1 :: Text -> Int
solve1 = fst . playUntil (Turn 2020) . pStartingNums

solve2 :: Text -> Int
solve2 = fst . playUntil (Turn 30000000) . pStartingNums
