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


main :: IO ()
main = do
    args <- parseArgsIO ArgsComplete [argumentDay, argumentInputFile]
    let day :: Int = getRequiredArg args 1
    let inputFile :: String = getRequiredArg args 2
    case day of
        1 -> putTextLn $ "Solution for day " <> show day <> "!"
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

    argumentInputFile :: Arg Int
    argumentInputFile = Arg
        { argIndex = 2
        , argAbbr = Just 'i'
        , argName = Just "input"
        , argData = argDataRequired "FILE" ArgtypeString
        , argDesc = "File containing the input of the problem."
        }
