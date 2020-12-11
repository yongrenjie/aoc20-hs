import Data.List (tails)
import Text.Read (readMaybe)
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d1.txt"
    input <- readFile fname
    putStr "Part 1 (2 integers which add to 2020): "
    print . getProdOf2Sums . parseInput $ input
    putStr "Part 2 (3 integers which add to 2020): "
    print . getProdOf3Sums . parseInput $ input

parseInput :: String -> [Integer]
parseInput = fromJust' . traverse readMaybe . words
    where fromJust' :: Maybe a -> a
          fromJust' (Just x) = x
          fromJust' Nothing = error "parseInput: some input was not a number"

{- |
- Takes a list of integers and returns all products of pairs which sum to 2020.
- Brute force approach, which is O(n^2).
- A more optimised approach would sort the list first and then perform binary search,
- which would be O(n lg n). However, a truly O(n lg n) implementation of that would
- require the use of Vectors, which I haven't quite gotten around to learning.
-}
getProdOf2Sums :: [Integer] -> [Integer]
getProdOf2Sums xs = [x1 * x2 | (x1:rest) <- tails xs,
                               x2 <- rest,
                               x1 + x2 == 2020]


{- |
- Takes a list of integers and returns all products of triples which sum to 2020.
- Brute force approach, which is O(n^3).
-}
getProdOf3Sums :: [Integer] -> [Integer]
getProdOf3Sums xs = [x1 * x2 * x3 | (x1:rest1) <- tails xs,
                                    (x2:rest2) <- tails rest1,
                                    x3 <- rest2,
                                    x1 + x2 + x3 == 2020]
