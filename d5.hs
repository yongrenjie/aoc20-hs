import Data.List
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d5.txt"
    input <- readFile fname
    let seatIDs = map (getSeatID . parseLine) . lines $ input
    putStr "Part 1: "
    print $ maximum seatIDs
    putStr "Part 2: "
    print $ findMissing seatIDs

type Row = Integer
type Col = Integer
type Seat = (Row, Col)

type Bounds = (Integer, Integer)
takeFirstHalf :: Bounds -> Bounds
takeFirstHalf (m, n) = let sp = n - m + 1 in (m, m + (sp `quot` 2) - 1)
takeSecondHalf :: Bounds -> Bounds
takeSecondHalf (m, n) = let sp = n - m + 1 in (m + (sp `quot` 2), n)

-- returns (row, column)
parseLine :: String -> Seat
parseLine cs = if minRow == maxRow && minCol == maxCol
                  then (minRow, minCol)
                  else error "incorrect input"
    where ((minRow, maxRow), (minCol, maxCol)) = foldl' halveBounds initialBounds cs
          initialBounds = ((0, 127), (0, 7))
          halveBounds :: (Bounds, Bounds) -> Char -> (Bounds, Bounds)
          halveBounds (rows, cols) 'F' = (takeFirstHalf rows, cols)
          halveBounds (rows, cols) 'B' = (takeSecondHalf rows, cols)
          halveBounds (rows, cols) 'L' = (rows, takeFirstHalf cols)
          halveBounds (rows, cols) 'R' = (rows, takeSecondHalf cols)
          halveBounds _ _ = error "incorrect input"

getSeatID :: Seat -> Integer
getSeatID (row, col) = row * 8 + col

findMissing :: [Integer] -> Integer
findMissing ids = incrSearch minID sortedIDs
    where sortedIDs = sort ids
          minID = minimum sortedIDs
          incrSearch :: Integer -> [Integer] -> Integer
          incrSearch target [] = target
          incrSearch target (x:xs) = if x == target
                                        then incrSearch (target + 1) xs
                                        else target
