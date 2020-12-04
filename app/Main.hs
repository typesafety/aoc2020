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

import Misc.Misc (Part (P1, P2))

import qualified Solutions.Day1 as D1
import qualified Solutions.Day2 as D2
import qualified Solutions.Day3 as D3
import qualified Solutions.Day4 as D4


main :: IO ()
main = do
    args <- parseArgsIO ArgsComplete [argumentDay, argumentPart, argumentInputFile]

    part <- case getRequiredArg args 2 :: Int of
                1 -> pure P1
                2 -> pure P2
                _ -> putTextLn "Invalid part." >> exitFailure

    let day :: Int = getRequiredArg args 1
    let inputFile :: String = getRequiredArg args 3
    case day of
        1 -> D1.solveIO part inputFile
        2 -> D2.solveIO part inputFile
        3 -> D3.solveIO part inputFile
        4 -> D4.solveIO part inputFile
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
