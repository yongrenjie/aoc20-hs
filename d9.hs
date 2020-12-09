-- I'm very, very tired and don't have the energy to look for a more efficient
-- algorithm or data structure today.
--
import Data.List
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d9.txt"
    input <- readFile fname
    let badSum = findBadSum 25 (map read . lines $ input)
    putStrLn $ "Part 1: " ++ show badSum
    let addsToBadSum = head $ findSubsequence badSum (map read . lines $ input)
    putStrLn $ "Part 2: " ++ show (minimum addsToBadSum + maximum addsToBadSum)

findBadSum :: Integer -> [Integer] -> Integer
findBadSum _ []     = error "whoops, should never reach end of list"
findBadSum n (x:xs) = go [] (x:xs)
    where go :: [Integer] -> [Integer] -> Integer
          go _ []     = error "whoops, should never reach end of list"
          go s (x:xs) = case fromIntegral (length s) < n of
                             True  -> go (s ++ [x]) xs
                             False -> if isPossibleSum x s
                                         then go (drop 1 s ++ [x]) xs
                                         else x

isPossibleSum :: Integer -> [Integer] -> Bool
isPossibleSum x s = x `elem` allSums
    where allSums = [a + b | (a:bs) <- tails s, b <- bs]

findSubsequence :: Integer -> [Integer] -> [[Integer]]
findSubsequence target ints = [subseq | t <- tails ints,
                                        subseq <- inits t,
                                        sum subseq == target]
