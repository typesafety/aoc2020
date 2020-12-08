{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day8
    ( solve1
    , solve2
    ) where

import Data.Text.Read (decimal, signed)
import Lens.Micro.Platform (modifying, use, view, makeLenses)
import Relude.Extra (bimapBoth)
import qualified Relude.Unsafe as Unsafe

import qualified Data.Text as T
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S


type Prog = IM.IntMap Instr

data Instr = Instr Opr Arg
    deriving (Show, Read)

data Opr = Acc | Jmp | Nop
    deriving (Show, Read)

newtype Arg = Arg Int
    deriving (Show, Read)

data St = St
    { _stAcc     :: Int
    , _stPc      :: Int
    , _stVisited :: Set Int
    } deriving (Show, Read, Ord, Eq)

$(makeLenses ''St)

initialState :: St
initialState = St
    { _stAcc = 0
    , _stPc = 0
    , _stVisited = S.empty
    }

type Execute = ReaderT Prog (State St)

solve1 :: Text -> Int
solve1 = view stAcc . snd . runProg . Unsafe.fromJust . pProg

solve2 :: Text -> Int
solve2 =
    view stAcc
    . Unsafe.head
    . mapMaybe (fst . runProg)
    . mutations
    . Unsafe.fromJust
    . pProg

mutations :: Prog -> [Prog]
mutations prog = map (mutate prog) . S.toList . view stVisited . snd . runProg $ prog
  where
    mutate :: Prog -> Int -> Prog
    mutate p idx = IM.adjust (\ (Instr opr arg) -> Instr (tl opr) arg) idx p
      where
        tl :: Opr -> Opr
        tl = \case
            Jmp -> Nop
            Nop -> Jmp
            x   -> x

runProg :: Prog -> (Maybe St, St)
runProg prog = runState (runReaderT execute prog) initialState
  where
    execute :: Execute (Maybe St)
    execute =
        finished >>= \case
            True  -> Just <$> get
            False -> stuck >>= \case
                True  -> pure Nothing
                False -> step >> execute
      where
        finished :: Execute Bool
        finished = not <$> (IM.member <$> use stPc <*> ask)

        stuck :: Execute Bool
        stuck = S.member <$> gets _stPc <*> gets _stVisited

    step :: Execute ()
    step = do
        pc <- use stPc
        modifying stVisited $ S.insert pc
        ask >>= runInstr . (IM.! pc)
      where
        runInstr :: Instr -> Execute ()
        runInstr (Instr i (Arg n)) = do
            stAcc `modifying` case i of
                Acc -> (+ n)
                _   -> id
            stPc `modifying` case i of
                Jmp -> (+ n)
                _   -> (+ 1)

pProg :: Text -> Maybe Prog
pProg = fmap (IM.fromList . zip [0 ..]) . mapM pInstr . lines
  where
    pInstr :: Text -> Maybe Instr
    pInstr =
        (\ (a, b) -> Instr <$> a <*> b)
        . bimap pOpr pArg
        . bimapBoth T.strip
        . T.breakOn " "
      where
        pOpr :: Text -> Maybe Opr
        pOpr = \case
            "acc" -> Just Acc
            "jmp" -> Just Jmp
            "nop" -> Just Nop
            _     -> Nothing

        pArg :: Text -> Maybe Arg
        pArg = fmap (Arg . fst) . rightToMaybe . signed decimal
