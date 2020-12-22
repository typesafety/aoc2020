module Main
    ( main
    ) where

import System.Console.ParseArgs
    ( Arg (..)
    , ArgsComplete (..)
    , Argtype (..)
    , argDataRequired
    , getRequiredArg
    , parseArgsIO
    )

import qualified Misc.Misc as Misc

import qualified Solutions.Day1 as D1
import qualified Solutions.Day2 as D2
import qualified Solutions.Day3 as D3
import qualified Solutions.Day4 as D4
import qualified Solutions.Day5 as D5
import qualified Solutions.Day6 as D6
import qualified Solutions.Day7 as D7
import qualified Solutions.Day8 as D8
import qualified Solutions.Day9 as D9
import qualified Solutions.Day10 as D10
import qualified Solutions.Day11 as D11
import qualified Solutions.Day12 as D12
import qualified Solutions.Day13 as D13
import qualified Solutions.Day15 as D15
import qualified Solutions.Day17 as D17


main :: IO ()
main = do
    args <- parseArgsIO ArgsComplete [argumentDay, argumentPart, argumentInputFile]

    part <- case getRequiredArg args 2 :: Int of
                1 -> pure Misc.P1
                2 -> pure Misc.P2
                _ -> putTextLn "Invalid part." >> exitFailure

    let day :: Int = getRequiredArg args 1
    let inputFile :: String = getRequiredArg args 3
    case day of
        1 -> D1.solveIO part inputFile
        2 -> D2.solveIO part inputFile
        3 -> D3.solveIO part inputFile
        4 -> D4.solveIO part inputFile
        5 -> D5.solveIO part inputFile
        6 -> Misc.solveIO (D6.solve1, D6.solve2) part inputFile
        7 -> Misc.solveIO (D7.solve1, D7.solve2) part inputFile
        8 -> Misc.solveIO (D8.solve1, D8.solve2) part inputFile
        9 -> Misc.solveIO (D9.solve1, D9.solve2) part inputFile
        10 -> Misc.solveIO (D10.solve1, D10.solve2) part inputFile
        11 -> Misc.solveIO (D11.solve1, D11.solve2) part inputFile
        12 -> Misc.solveIO (D12.solve1, D12.solve2) part inputFile
        13 -> Misc.solveIO (D13.solve1, D13.solve2) part inputFile
        15 -> Misc.solveIO (D15.solve1, D15.solve2) part inputFile
        17 -> Misc.solveIO (D17.solve1, D17.solve2) part inputFile
        _ -> putTextLn "Invalid day." >> exitFailure

  where
    argumentDay :: Arg Int
    argumentDay = Arg
        { argIndex = 1
        , argAbbr = Just 'd'
        , argName = Just "day"
        , argData = argDataRequired "DAY" ArgtypeInt
        , argDesc = "Day of solution (1-25)."
        }

    argumentPart :: Arg Int
    argumentPart = Arg
        { argIndex = 2
        , argAbbr = Just 'p'
        , argName = Just "part"
        , argData = argDataRequired "PART" ArgtypeInt
        , argDesc = "Part of solution (1 or 2)"
        }

    argumentInputFile :: Arg Int
    argumentInputFile = Arg
        { argIndex = 3
        , argAbbr = Just 'i'
        , argName = Just "input"
        , argData = argDataRequired "FILE" ArgtypeString
        , argDesc = "File containing the input of the problem."
        }
