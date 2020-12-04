import Data.List (foldl')
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d3.txt"
    input <- readFile fname
    let forestLines = map parseLine . lines $ input
    let count11 = countTrees 1 1 forestLines
    let count31 = countTrees 3 1 forestLines
    let count51 = countTrees 5 1 forestLines
    let count71 = countTrees 7 1 forestLines
    let count12 = countTrees 1 2 forestLines
    putStrLn $ "right 1, down 1: " ++ show count11
    putStrLn $ "right 3, down 1: " ++ show count31
    putStrLn $ "right 5, down 1: " ++ show count51
    putStrLn $ "right 7, down 1: " ++ show count71
    putStrLn $ "right 1, down 2: " ++ show count12
    putStrLn $ "product: " ++ show (product [count11, count31, count51, count71, count12])


type XCoord = Integer
type YCoord = Int

{-|
- Record type for one "line" of a forest. width is the number of squares each
- line spans in the x-direction. trees is a list containing the (0-indexed)
- x-coordinates where there are trees. For example, one line of a forest
-        .#.#.##
- would be represented as ForestLine 7 [1, 3, 5, 6].
-}
data ForestLine = ForestLine { width :: Integer
                             , trees :: [XCoord]
                             } deriving Show

{-|
- Given a (0-indexed) x-coordinate and a ForestLine, determine whether there is
- a tree at that x-coordinate.
-}
isTree :: XCoord -> ForestLine -> Bool
isTree n fl = (n `rem` width fl) `elem` trees fl

{-|
- Parse a line of input into a ForestLine.
-}
parseLine :: String -> ForestLine
parseLine cs = ForestLine (fst t) (snd t)
    where t = foldl' accum (0, []) cs
          accum :: (XCoord, [XCoord]) -> Char -> (XCoord, [XCoord])
          accum (a, xs) '#' = (a+1, a:xs)
          accum (a, xs) _   = (a+1, xs)


{-|
- Counts the number of trees you'll bump into, given the number of squares to
- move right (dx), the number of squares to move down (dy), and a list of
- ForestLines.
-}
countTrees  :: XCoord -> YCoord -> [ForestLine] -> Int
countTrees dx dy allLines = length . filter id $ zipWith isTree xCoords filteredLines
    where xCoords = [0,dx..]
          filteredLines = [allLines !! y | y <- enumFromThenTo 0 dy (length allLines - 1)]
