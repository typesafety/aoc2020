module Solutions.Day4
    ( solveIO
    , solve1
    ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import qualified Text.Read

import Data.Char (isDigit)

import Misc.Misc (Part (..))


type Passport = M.Map Text Text

solveIO :: Part -> FilePath -> IO ()
solveIO part fp = readFileText fp >>= print . solver
  where
    solver :: (Text -> Int)
    solver = case part of
        P1 -> solve1
        P2 -> solve2

solve1 :: Text -> Int
solve1 = length . filter isValid . parsePassports

solve2 :: Text -> Int
solve2 = length . filter isValid2 . parsePassports

parsePassports :: Text -> [Passport]
parsePassports = map parsePassport . T.splitOn "\n\n"
  where
    parsePassport :: Text -> Passport
    parsePassport = assemble . map (unsafeBreakOn ":") . words

    assemble :: [(Text, Text)] -> Passport
    assemble = M.fromList

isValid :: Passport -> Bool
isValid pp = all (`M.member` pp) (allFields Set.\\ optFields)
  where
    optFields :: Set Text
    optFields = Set.fromList ["cid"]

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

isValid2 :: Passport -> Bool
isValid2 pp = isValid pp && all validReqField (M.toList pp)

validReqField :: (Text, Text) -> Bool
validReqField = \case
    ("byr", val) -> T.length val == 4 && ((unsafeRead val :: Int) `elem` [1920 .. 2002])
    ("iyr", val) -> T.length val == 4 && ((unsafeRead val :: Int) `elem` [2010 .. 2020])
    ("eyr", val) -> T.length val == 4 && ((unsafeRead val :: Int) `elem` [2020 .. 2030])

    ("hgt", val) -> case T.span isDigit val of
        (num, "cm") -> (unsafeRead num :: Int) `elem` [150 .. 193]
        (num, "in") -> (unsafeRead num :: Int) `elem` [59 .. 76]
        _           -> False

    ("hcl", val) -> case T.breakOnEnd "#" val of
        ("#", colorCode) -> T.length val == 7 && T.all (`elem` hexChars) colorCode
        _ -> False

    ("ecl", val) -> val `elem` eyeColors

    ("pid", val) -> T.length val == 9 && T.all isDigit val

    -- Any other fields defaults to valid.
    _ -> True

  where
    hexChars :: String
    hexChars = "0123456789abcdef" :: String

    eyeColors :: [Text]
    eyeColors = ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

unsafeBreakOn :: Text -> Text -> (Text, Text)
unsafeBreakOn delim input = case second (T.stripPrefix delim) $ T.breakOn delim input of
    (_, Nothing) -> error "unsafeBreakOn: delimiter not found"
    (before, Just after) -> (before, after)

unsafeRead :: Read a => Text -> a
unsafeRead = Text.Read.read . toString
