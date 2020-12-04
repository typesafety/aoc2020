module Solutions.Day2
    ( solveIO
    , solve
    ) where

import Relude.Extra (bimapBoth)

import qualified Text.Read
import qualified Data.Text as T


data Policy = Policy
    { policyOccurrence :: (Int, Int)
    , policyChar :: Char
    }

newtype Password = Password
    { unPassword :: Text
    }


solveIO :: FilePath -> IO ()
solveIO fp = readFileText fp >>= print . solve

solve :: Text -> Int
solve = length . filter id . map (uncurry isValid . parseEntry) . lines

isValid :: Policy -> Password -> Bool
isValid (Policy (minOcc, maxOcc) char) =
    (`elem` [minOcc .. maxOcc])
    . T.length
    . T.filter (== char)
    . unPassword

-- | Parse a line of text and return the password policy and the password.
parseEntry :: Text -> (Policy, Password)
parseEntry = bimap pPolicy pPassword . unsafeBreakOn ":"
  where
    pPassword :: Text -> Password
    pPassword = Password . T.strip

    pPolicy :: Text -> Policy
    pPolicy = uncurry Policy . bimap pOccurrence pLetter . unsafeBreakOn " "
      where
        pOccurrence :: Text -> (Int, Int)
        pOccurrence = bimapBoth unsafeRead . unsafeBreakOn "-"

        pLetter :: Text -> Char
        pLetter = T.head

{- | Splits the input on the delimiter, and discards the delimiter.
Crashes if no delimiter is found in the input text. -}
unsafeBreakOn :: Text -> Text -> (Text, Text)
unsafeBreakOn delim input = case second (T.stripPrefix delim) $ T.breakOn delim input of
    (_, Nothing) -> error "unsafeBreakOn: delimiter not found"
    (before, Just after) -> (before, after)

unsafeRead :: Read a => Text -> a
unsafeRead = Text.Read.read . toString
