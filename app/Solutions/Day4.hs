module Solutions.Day4
    ( solveIO
    , solve1
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as Set

import Misc.Misc (Part (..))


type Passport = M.Map Text Text

solveIO :: Part -> FilePath -> IO ()
solveIO part fp = readFileText fp >>= print . solver
  where
    solver :: (Text -> Int)
    solver = case part of
        P1 -> solve1
        P2 -> error "Not yet solved"

solve1 :: Text -> Int
solve1 = length . filter isValid . parsePassports

parsePassports :: Text -> [Passport]
parsePassports = map parsePassport . T.splitOn "\n\n"
  where
    parsePassport :: Text -> Passport
    parsePassport = assemble . map (T.breakOn ":") . words

    assemble :: [(Text, Text)] -> Passport
    assemble = M.fromList

isValid :: Passport -> Bool
isValid pp = all (`M.member` pp) (allFields Set.\\ optFields)
  where
    allFields :: Set Text
    allFields = Set.fromList
        [ "byr"
        , "iyr"
        , "eyr"
        , "hgt"
        , "hcl"
        , "ecl"
        , "pid"
        , "cid"
        ]

    optFields :: Set Text
    optFields = Set.fromList ["cid"]
