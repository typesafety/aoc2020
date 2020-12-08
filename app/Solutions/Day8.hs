{-# LANGUAGE TemplateHaskell #-}

module Solutions.Day8
    ( solve1
    , solve2
    ) where

import Data.Text.Read (decimal, signed)
import Lens.Micro.Platform (modifying, use, view, makeLenses)
import Relude.Extra (bimapBoth)

import Misc.Misc (if')

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
    }

type RunProg = ReaderT Prog (State St)

$(makeLenses ''St)

solve1 :: Text -> Int
solve1 = maybe (error "💩") (view stAcc . runProg) . pProg

solve2 :: Text -> Int
solve2 = error "Not yet solved"

runProg :: Prog -> St
runProg = flip execState initialState . runReaderT execute
  where
    initialState :: St
    initialState = St
        { _stAcc = 0
        , _stPc = 0
        , _stVisited = S.empty
        }

execute :: RunProg ()
execute =
    (,) <$> use stPc <*> use stVisited
    >>= flip (`if'` pass) (step >> execute) . uncurry S.member  -- lololol
  where
    step :: RunProg ()
    step = do
        pc <- use stPc
        modifying stVisited $ S.insert pc
        ask >>= runInstr . (IM.! pc)
      where
        runInstr :: Instr -> RunProg ()
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
