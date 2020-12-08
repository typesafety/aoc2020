module Solutions.Day7
    ( solve1
    , solve2
    ) where

import Relude.Extra (prev)
import Text.Read (read)

import qualified Data.Map.Strict as M
import qualified Data.Text as T


solve1 :: Text -> Int
solve1 = runReader (numValid "shiny gold") . pRules

solve2 :: Text -> Int
solve2 = runReader (numTotal "shiny gold") . pRules

type Color = Text
type Rules = M.Map Color [(Int, Color)]

numTotal :: Color -> Reader Rules Int
numTotal target = sum <$> (mapM qtyRecursive . (M.! target) =<< ask)

qtyRecursive :: (Int, Color) -> Reader Rules Int
qtyRecursive (qty, bagClr) =  -- lolololololol
    maybe (pure qty) (fmap ((+ qty) . sum . map (* qty)) . mapM qtyRecursive)
    . guarded (not . null)
    . (M.! bagClr)
    =<< ask

numValid :: Color -> Reader Rules Int
numValid target = do
    rules <- ask
    prev . length . filter id <$> mapM (search target) (M.keys rules)
  where
    search :: Color -> Color -> Reader Rules Bool
    search targetClr bagClr
        | bagClr == targetClr = pure True
        | otherwise = anyM (search targetClr . snd) . (M.! bagClr) =<< ask

pRules :: Text -> Rules
pRules = M.fromList . map pRule . lines
  where
    pRule :: Text -> (Color, [(Int, Color)])
    pRule = bimap T.strip pInners . separate
      where
        separate :: Text -> (Text, Text)
        separate =
            T.breakOn "contain"
            . T.strip
            . T.replace "no other" ""
            . T.replace "bag" ""
            . T.replace "bags" ""
            . T.filter (/= '.')

        pInners :: Text -> [(Int, Color)]
        pInners =
            map pInner
            . filter (not . T.null)
            . map T.strip
            . T.splitOn ","
            . T.replace "contain" ""

        pInner :: Text -> (Int, Color)
        pInner = bimap (read . toString) T.strip . T.breakOn " "
