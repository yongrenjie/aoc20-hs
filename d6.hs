{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import Data.List
import Data.List.Split (splitOn)
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d6.txt"
    input <- readFile fname
    putStr "Part 1: "
    print $ sum . map countAny . getGroups $ input
    putStr "Part 2: "
    print $ sum . map countAll . getGroups $ input

getGroups :: String -> [String]
getGroups = splitOn "\n\n"

countAny :: String -> Integer
countAny s = go 0 (sort . filter isLetter $ s)
    where go :: Integer -> String -> Integer
          go n []              = n
          go n [_]             = n + 1
          go n (c:cs@(next:_)) = if c == next
                                       then go n cs
                                       else go (n + 1) cs

getResponseCounts :: String -> [Int]
getResponseCounts = map length . group . sort . filter isLetter

countAll :: String -> Int
countAll response = length . filter (== l) . getResponseCounts $ response
    where l = length (lines response)
