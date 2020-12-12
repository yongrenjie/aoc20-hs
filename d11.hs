import Control.Monad (guard)
import Data.Char
import Data.List.Split (chunksOf)
import Data.Map (Map)
import qualified Data.Map as M
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d11.txt"
    layout <- parseInput <$> readFile fname
    putStr "Part 1: "
    print $ countOccupied . (runUntilUnchanged 1) $ layout
    putStr "Part 2: "
    print $ countOccupied . (runUntilUnchanged 2) $ layout

data Seat = Occupied | Empty | Floor deriving (Eq, Show)

-- The seat layout is stored as a tuple of nrows, ncols, and a Map which maps
-- (zero-indexed) (row, col) tuples to the Seat status.
type Layout = (Int, Int, Map (Int, Int) Seat)

-- Parses the input into an instance of the seat layout.
parseInput :: String -> Layout
parseInput input = (nrows, ncols, initialState)
    where noSpaces = filter (not . isSpace) $ input
          ncols = length $ takeWhile (`elem` "#L.") input
          nrows = length noSpaces `div` ncols
          initialState = M.fromList . zip seats . map conv $ noSpaces
          seats = [(row, col) | row <- [0..nrows-1], col <- [0..ncols-1]]
          conv :: Char -> Seat
          conv '#' = Occupied
          conv 'L' = Empty
          conv '.' = Floor
          conv _ = error "invalid input"

-- This performs one "update" cycle of the seat layout based on the rules of
-- either Part 1 or 2.
runRules :: Int -> Layout -> Layout
runRules part (nrows, ncols, oldState) = (nrows, ncols, newState)
    where
        -- The minimum number of occupied neighbouring seats that cause people
        -- to vacate their occupied seats.
        tol = case part of
                   1 -> 4
                   2 -> 5
                   _ -> error "part can only be 1 or 2"
        -- Updating the seat layout is as simple as updating each seat and
        -- creating a new Map from the updated data.
        newState = M.fromList . map (\seat -> (seat, update seat)) $ seats
        seats = [(row, col) | row <- [0..nrows-1], col <- [0..ncols-1]]
        -- Check what the seat should be in the next cycle.
        update seat = case M.lookup seat oldState of
                           Just Floor    -> Floor
                           Just Occupied -> if adjOccupied seat >= tol then Empty else Occupied
                           Just Empty    -> if adjOccupied seat == 0 then Occupied else Empty
                           -- Because we know what the seats are (and can
                           -- generate them exhaustively through a list
                           -- comprehension), update should never be called on
                           -- a seat that doesn't exist.
                           Nothing       -> error "you shouldn't have done this"
        -- Counts the number of 'adjacent' seats which are occupied. The
        -- function used to determine adjacency depends on whether it's Part 1
        -- or 2.
        adjOccupied :: (Int, Int) -> Int
        adjOccupied = length                           -- count number of occupied seats
                      . filter (== (Just Occupied))    -- check which of them are occupied
                      . map (flip M.lookup oldState)   -- find the state of the seats
                      . adjacencyFn                    -- get all neighbouring seats
        adjacencyFn = case part of
                           1 -> immediateNeighbours
                           2 -> lineOfSightNeighbours
                           _ -> error "part can only be 1 or 2"
        -- Returns immediate neighbours of a seat. Includes out of bounds seats
        -- since looking them up will return Nothing, and we filter for Just
        -- Occupied.
        immediateNeighbours :: (Int, Int) -> [(Int, Int)]
        immediateNeighbours (r, c) = do
            dr <- [-1..1]
            dc <- [-1..1]
            guard $ (dr /= 0 || dc /= 0)
            return (r + dr, c + dc)
        -- Returns line-of-sight neighbours of a seat. Includes out of bounds
        -- seats since looking them up will return Nothing, and we filter for
        -- Just Occupied.
        lineOfSightNeighbours :: (Int, Int) -> [(Int, Int)]
        lineOfSightNeighbours (r, c) = do
            dr <- [-1..1]
            dc <- [-1..1]
            guard $ (dr /= 0 || dc /= 0)
            return $ getNextLineOfSight (dr, dc) (r, c)
        -- Returns the line-of-sight neighbour of a seat (r, c) in a direction
        -- (dr, dc). This can return out-of-bounds neighbours if there are no
        -- line-of-sight neighbours within bounds. However, as stated above,
        -- that's not an issue.
        getNextLineOfSight :: (Int, Int) -> (Int, Int) -> (Int, Int)
        getNextLineOfSight (dr, dc) (r, c)
            | r + dr < 0                           = next
            | c + dc < 0                           = next
            | r + dr > nrows - 1                   = next
            | c + dc > ncols - 1                   = next
            | M.lookup next oldState == Just Floor = getNextLineOfSight (dr, dc) next
            | otherwise                            = next
            where next = (r + dr, c + dc)

-- Counts the number of occupied seats in a layout.
countOccupied :: Layout -> Int
countOccupied (_, _, m) = length $ M.filter (== Occupied) m

-- Recursively runs game rules on a layout until the next layout is the same as
-- the current one. The `part` parameter is 1 or 2 for Part 1 and Part 2
-- respectively.
runUntilUnchanged :: Int -> Layout -> Layout
runUntilUnchanged part x = let x' = runRules part x
                               in if x' == x then x else runUntilUnchanged part x'
-- Alternative definition nicked from /u/veydar_ on Reddit (but I like mine a
-- bit better)
-- runUntilUnchanged part layout = fst . head . dropWhile (not . snd) $ iterate propagate (layout, False)
--     where propagate (x, _) = let x' = runRules part x in (x', x' == x)

-- The inverse of parseInput. For debugging.
showMap :: Layout -> String
showMap (nrows, ncols, state) = unlines
                                . chunksOf ncols
                                . map (showSeat . flip M.lookup state)
                                $ seats
    where showSeat (Just Occupied) = '#'
          showSeat (Just Empty)    = 'L'
          showSeat (Just Floor)    = '.'
          showSeat _               = error "you shouldn't have done this"
          seats = [(row, col) | row <- [0..nrows-1], col <- [0..ncols-1]]
