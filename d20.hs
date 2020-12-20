import Debug.Trace (trace)
import           Data.Char                      ( isDigit )
import qualified Data.Vector                   as V
import qualified Data.Matrix                   as M
import qualified Data.IntMap                   as IM
import           Data.List                      ( foldl1' )
import           Data.List.Split                ( splitOn )
import           Paths_aoc20_hs

main :: IO ()
main = do
  fname <- getDataFileName "d20s.txt"
  input <- readFile fname
  
  putStr "Part 1: "
  let tiles = map parseTile . splitOn "\n\n" $ input
      cornerIDs = map idnum $ filter ((==2) . countPairedEdges tiles) tiles
  print $ product cornerIDs

  -- For part 2, it looks like we actually need to solve it. :-/
  putStr "Part 2: "
  -- Arbitrariily choose one of the corner tiles to be the top-left corner.
  let topLeftCorner  = head $ filter ((`elem` cornerIDs) . idnum) tiles
  -- Orient it in a correct way such that the edges that can match are on
  -- the outside.
      topLeftCornerO = orientTopLeftCorner tiles topLeftCorner
  -- Solve the first column using this top-left corner.
      leftColumnTiles = solveColumn tiles topLeftCornerO
  -- Remove the used tiles from the list of tiles.
      remainingTiles = [t | t <- tiles , not $ idnum t `elem` map idnum leftColumnTiles]
  -- Solve each row using the tiles previously found.
      solvedTiles = map (solveRow remainingTiles) leftColumnTiles
  -- Now combine all of the inner bits together into a gigantic matrix...
      inners      = map (map inner) solvedTiles
      bigPicture  = foldl1' (M.<->) . map (foldl1' (M.<|>)) $ inners


type Edge = V.Vector Int
data Tile = Tile
  { idnum :: Int
  , north, east, south, west :: Edge
  , inner :: M.Matrix Int
  }
  deriving (Eq, Show)

-- Return all possible edges of a tile.
getAllEdges :: Tile -> [Edge]
getAllEdges tile = [f (g tile) | f <- [id, V.reverse]
                               , g <- [north, east, south, west]]

parseTile :: String -> Tile
parseTile t = Tile i n e s w x
  where
    (topLine:rest) = lines t
    rest01s = map (map (\c -> if c == '#' then 1 else 0)) rest
    i = read $ filter isDigit topLine
    n = V.fromList $ head rest01s
    e = V.fromList $ map last rest01s
    s = V.fromList $ reverse (last rest01s)
    w = V.fromList $ reverse (map head rest01s)
    x = M.fromLists $ map (init . tail) (init . tail $ rest01s)

-- Counts the number of edges of a tile that can match with a set of tiles.
-- If this number is 2, then the tile is a corner tile. If it's 3, then it's an
-- edge. If it's 4, then it's in the middle somewhere.
countPairedEdges :: [Tile] -> Tile -> Int
countPairedEdges allTiles tile = n
  where
    allEdges      = [f t | f <- [north, south, east, west]
                         , t <- allTiles
                         , idnum t /= idnum tile]
    searchQueries = [f (g tile) | f <- [id, V.reverse]
                                , g <- [north, south, east, west]]
    searchHits    = map (\q -> length $ filter (==q) allEdges) searchQueries
    n             = sum searchHits

-- Flip a tile horizontally
flipT :: Tile -> Tile
flipT (Tile i n e s w x) = Tile i n' e' s' w' x'
  where
    n' = V.reverse n
    e' = V.reverse w
    s' = V.reverse s
    w' = V.reverse e
    -- Hardcode the size.
    x' = M.switchCols 1 8 . M.switchCols 2 7 . M.switchCols 3 6 . M.switchCols 4 5 $ x
-- Rotate a tile by 90 degrees clockwise.
rotateT :: Tile -> Tile
rotateT (Tile i n e s w x) = Tile i w n e s x'
  where
    x' = M.switchCols 1 8 . M.switchCols 2 7 . M.switchCols 3 6 . M.switchCols 4 5 . M.transpose $ x

-- -- Piece the tiles together.
-- solve :: [Tile] -> M.Matrix Int
-- solve tiles = m
--   where
--     m = M.identity 3
--     tileMap = IM.fromList $ map (\t -> (idnum t, t)) tiles
--     getTile = (tileMap IM.!)

solveRow :: [Tile] -> Tile -> [Tile]
solveRow allTiles leftmost = result
  where
    result = take 3 $ iterate (getNextTile allTiles) leftmost
    getNextTile :: [Tile] -> Tile -> Tile
    getNextTile allTiles leftTile = rightTileO
      where
        otherTiles   = [t | t <- allTiles, idnum t /= idnum leftTile]
        edgeToMatch  = east leftTile
        -- we assume here that only one tile can match
        rightTile    = head $ filter (matchT edgeToMatch) otherTiles
        rightTileO   = orientT edgeToMatch rightTile
    -- `matchT edge tile` checks whether the given tile can possibly be
    -- oriented in such a way that matches the given edge.
    matchT :: Edge -> Tile -> Bool
    matchT target tile = target `elem` getAllEdges tile
    -- Assuming we have found a tile that goes on the *right* of a target
    -- edge, `orientT target tile` will return tile correctly oriented.
    orientT :: Edge -> Tile -> Tile
    orientT target tile
      | target == V.reverse (west tile)  = tile
      | target == V.reverse (south tile) = rotateT tile
      | target == V.reverse (east tile)  = rotateT . rotateT $ tile
      | target == V.reverse (north tile) = rotateT . rotateT . rotateT $ tile
      | target == (west tile)            = flipT . rotateT . rotateT $ tile
      | target == (south tile)           = rotateT . flipT $ tile
      | target == (east tile)            = flipT tile
      | target == (north tile)           = flipT . rotateT $ tile
      | otherwise                        = error "uh oh"

solveColumn :: [Tile] -> Tile -> [Tile]
solveColumn allTiles topmost = result
  where
    -- This code is the same as solveRow except that the directions are changed.
    result = take 3 $ iterate (getNextTile allTiles) topmost
    getNextTile :: [Tile] -> Tile -> Tile
    getNextTile allTiles topTile = bottomTileO
      where
        otherTiles   = [t | t <- allTiles, idnum t /= idnum topTile]
        edgeToMatch  = south topTile
        -- we assume here that only one tile can match
        bottomTile   = head $ filter (matchT edgeToMatch) otherTiles
        bottomTileO  = orientT edgeToMatch bottomTile
    matchT :: Edge -> Tile -> Bool
    matchT target tile = target `elem` getAllEdges tile
    orientT :: Edge -> Tile -> Tile
    orientT target tile
      | target == V.reverse (north tile) = tile
      | target == V.reverse (west tile)  = rotateT tile
      | target == V.reverse (south tile) = rotateT . rotateT $ tile
      | target == V.reverse (east tile)  = rotateT . rotateT . rotateT $ tile
      | target == (north tile)           = flipT tile
      | target == (west tile)            = flipT . rotateT $ tile
      | target == (south tile)           = flipT . rotateT . rotateT $ tile
      | target == (east tile)            = rotateT . flipT $ tile
      | otherwise                        = error "uh oh"

-- Orient the top-left corner such that the edges that match other tiles
-- are facing the east and south.
orientTopLeftCorner :: [Tile] -> Tile -> Tile
orientTopLeftCorner allTiles tile = tile'
  where
    allOtherEdges :: [Edge]
    allOtherEdges = [f (g t) | f <- [id, V.reverse]
                             , g <- [north, south, east, west]
                             , t <- allTiles
                             , idnum t /= idnum tile]
    nIn, eIn, sIn, wIn :: Bool
    [nIn, eIn, sIn, wIn] = map (\f -> f tile `elem` allOtherEdges) [north, east, south, west]
    tile' | sIn && eIn = tile
          | wIn && sIn = rotateT . rotateT . rotateT $ tile
          | nIn && wIn = rotateT . rotateT $ tile
          | eIn && nIn = rotateT $ tile
