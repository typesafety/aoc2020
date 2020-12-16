module Solutions.Day15
    ( solve1
    , solve2
    ) where

import Relude.Extra (dup)

import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T

import qualified Misc.Misc as Misc


newtype Interval = Interval
    { unInterval :: Int
    } deriving Show

onInterval :: (Int -> Int) -> Interval -> Interval
onInterval f = Interval . f . unInterval

type History = IntMap Interval

start :: NonEmpty Int -> State History Interval
start ns =
    let (initNums, lastNum) :: ([Int], Int) = bimap init last $ dup ns
    in mapM_ speakConst initNums >> pure (Interval lastNum)
  where
    speakConst :: Int -> State History Interval
    speakConst toSpeak = do
        modify' (IM.map (onInterval (+ 1)) . IM.insert toSpeak (Interval 0))
        pure $ Interval toSpeak

speak :: Int -> State History Interval
speak lastNum = do
    toSpeak <- fromMaybe new <$> gets (IM.lookup lastNum)
    modify' (IM.map (onInterval (+ 1)) . IM.insert lastNum new)
    pure toSpeak
  where
    new :: Interval
    new = Interval 0

playTurns :: Int -> NonEmpty Int -> (Interval, History)
playTurns numTurns startingNums =
    usingState IM.empty
    $ start startingNums
        >>= Misc.applyNM (numTurns - length startingNums) (speak . unInterval)

pStartingNums :: Text -> NonEmpty Int
pStartingNums = fromList . map Misc.unsafeReadText . T.splitOn ","

solve1 :: Text -> Int
solve1 = unInterval . fst . playTurns 2020 . pStartingNums

solve2 :: Text -> Int
solve2 = error "Not yet solved"
