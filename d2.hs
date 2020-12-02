{-# LANGUAGE OverloadedStrings #-}

import Data.Char (isSpace)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d2.txt"
    input <- TIO.readFile fname
    let numberValid1 = processInput isValid1 input
    putStrLn $ "Part 1: there are " ++ show numberValid1 ++ " valid passwords"
    let numberValid2 = processInput isValid2 input
    putStrLn $ "Part 2: there are " ++ show numberValid2 ++ " valid passwords"


-- The record names here only really make sense for the first question, but well.
data PasswordChecker = PChecker { minchar :: Int
                                , maxchar :: Int
                                , char :: Char
                                , password :: T.Text
                                }

{-|
- Returns True if the PasswordChecker instance satisfies the conditions specified
- in the first part of the question.
- I wonder if it would be more elegant to use Reader monad instead of records.
-}
isValid1 :: PasswordChecker -> Bool
isValid1 pc = nchar >= minchar pc && nchar <= maxchar pc
    where nchar = T.length . T.filter ((==) $ char pc) $ password pc

-- |A plain exclusive or function.
xor :: Bool -> Bool -> Bool
xor True True = False
xor False False = False
xor _ _ = True

{-|
- Returns True if the PasswordChecker instance satisfies the conditions specified
- in the second part of the question.
-}
isValid2 :: PasswordChecker -> Bool
isValid2 pc = (firstLookupChar == targetChar) `xor` (secondLookupChar == targetChar)
    where firstLookupChar = T.index (password pc) (minchar pc - 1)
          secondLookupChar = T.index (password pc) (maxchar pc - 1)
          targetChar = char pc

{-|
- Parse one line of input into a PasswordChecker instance.
-}
parseLine :: T.Text -> PasswordChecker
parseLine line = PChecker minInt maxInt c pw
    where (leftHalf:rightHalf:_) = T.split (==':') line
          pw = T.strip rightHalf
          (nums:char:_) = T.split isSpace leftHalf
          c = T.head char
          (mins:maxs:_) = T.split (=='-') nums
          minInt = read $ T.unpack mins
          maxInt = read $ T.unpack maxs

{-|
- Find the number of valid passwords in an input
-}
processInput :: (PasswordChecker -> Bool) -> T.Text -> Int
processInput isValid = length . filter id . map isValid . map parseLine . T.lines
