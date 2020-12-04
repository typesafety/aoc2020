module Solutions.Day3 where

import qualified Data.Text as T


data Obj = Open | Tree
    deriving (Show)

showObj :: Obj -> Char
showObj = \case
    Open -> '.'
    Tree -> '#'

-- | Read an object from a Char, crash on invalid character.
readObj :: Char -> Obj
readObj = \case
    '.' -> Open
    '#' -> Tree
    _   -> error "readObj: invalid character"

{- | The map is a nested sequence. The inner sequence represents a row
row in on the map -}
type TreeRow = Seq Obj
type TreeGrid = Seq TreeRow

showGrid :: TreeGrid -> Text
showGrid = unlines . toList . fmap showRow

showRow :: TreeRow -> Text
showRow = fromString . toList . fmap showObj

parseGrid :: Text -> TreeGrid
parseGrid = fromList . map pRow . lines

pRow :: Text -> Seq Obj
pRow = fromList . map readObj . toString

