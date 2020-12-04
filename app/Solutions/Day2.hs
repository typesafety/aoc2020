module Solutions.Day2
    ( solveIO
    ) where

import Relude.Extra (bimapBoth)

import qualified Text.Read
import qualified Data.Text as T

import Misc.Misc (Part (..))


data Policy = Policy (Int, Int) Char

newtype Password = Password
    { unPassword :: Text
    }

newtype PassChecker = PassChecker (Policy -> Password -> Bool)

solveIO :: Part -> FilePath -> IO ()
solveIO part fp = readFileText fp >>= print . solve checker
  where
    checker :: PassChecker
    checker = case part of
        P1 -> checker1
        P2 -> checker2

solve :: PassChecker -> Text -> Int
solve (PassChecker checker) =
    length
    . filter id
    . map (uncurry checker . parseEntry)
    . lines

checker1 :: PassChecker
checker1 = PassChecker $ \ (Policy (minOcc, maxOcc) char) ->
    (`elem` [minOcc .. maxOcc])
    . T.length
    . T.filter (== char)
    . unPassword

checker2 :: PassChecker
checker2 = PassChecker $ \ (Policy (pos1, pos2) char) ->
    uncurry xor
    . (\ pw ->
        bimapBoth
            ((== char) . flip ($) pw)
            (flip T.index $ pos1 - 1, flip T.index $ pos2 - 1))
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
