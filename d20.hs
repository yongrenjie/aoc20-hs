import           Data.Char                      ( isDigit )
import           Data.List                      ( foldl'
                                                , foldl1'
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
import qualified Data.Matrix                   as M
import qualified Data.Vector                   as V
import           Paths_aoc20_hs

{-
- Overall strategy
- ----------------
-
- This puzzle was a real slog for me. The key assumption I made about the problem
- was that each edge can only line up with one other edge amongst all tiles (i.e.
- we would be able to identify matching edges unambiguously). The second assumption
- I made was that sea monsters (I call them serpents) did not overlap with each other.
-
- The method to piece together the jigsaw was largely as follows:
-
-  1. Convert (., #) to (0, 1) respectively. This is not needed for the jigsaw, but
-     it's handy for Step 9.
-
-  2. Identify which pieces have only two edges that can match with other tiles.
-     These must be the corner tiles. (This conveniently solves Part 1.)
-
-  3. Without loss of generality, choose one of them as the top-left corner tile.
-     Rotate it such that the edges that *can* match face inwards (i.e. to the east
-     and to the south).
-
-  4. Build the first column by iteratively searching for the tile to the bototm of
-     a given tile. When we cannot find any more tiles to go to the bottom, then we
-     know we have reached the end of the first column.
-
-  5. Build the rows of the puzzle by mapping Step 3 over each of the tiles in the
-     first column (except that the chains grow rightwards, not downwards).
-
-  6. Build the full matrix by stripping away the edges. In my case I stored the
-     edges in a separate record field from the inner bits. This matrix may not
-     necessarily be in the correct orientation to find serpents.
-
-  7. Build all possible serpent-sized submatrices from the matrix.
-
-  8. Build all possible orientations of the serpent. By checking the matrix against
-     every serpent orientation, we no longer need to flip / rotate the big matrix
-     itself.
-
-  9. Perform elementwise multiplication between the serpent and the submatrix. If 
-     the product is equal to the serpent, then there is a serpent in the submatrix
-     (this is rather like a 2D bitmask). Count the number of serpents.
-
- 10. Count the number of 1's in the matrix and subtract the number of serpents times
-     the number of 1's in a serpent for the answer to Part 2.
-
- Some parts of this would almost certainly have been easier using a more feature-rich
- library, such as Numpy. But plain old Vectors and Matrices were good enough this time.
-
-}


main :: IO ()
main = do
  fname <- getDataFileName "d20.txt"
  input <- readFile fname

  -- For Part 1, we can get away with not actually solving the whole thing.
  -- We simply need to find which of the tiles have exactly two edges that
  -- don't match any other tiles; those must be the corners.
  putStr "Part 1: "
  let tiles       = map parseTile . splitOn "\n\n" $ input
      cornerTiles = filter ((== 2) . countMatchableEdges tiles) tiles
  print $ product (map idnum cornerTiles)

  -- For part 2, it looks like we actually need to solve it. :-(
  putStr "Part 2: "
      -- Arbitrarily choose one of the corner tiles to be the top-left corner.
  let topLeftCorner   = head cornerTiles
      -- Orient it in a way such that the matchable edges are on the inside.
      topLeftCornerO  = orientTopLeftCorner tiles topLeftCorner
      -- Solve the first column using this top-left corner.
      leftColumnTiles = solveColumn tiles topLeftCornerO
      -- Solve each row using the tiles previously found.
      solvedTiles     = map (solveRow tiles) leftColumnTiles
      -- Now combine all of the inner bits together into a gigantic matrix...
      inners          = map (map inner) solvedTiles
      bigPicture      = foldl1' (M.<->) . map (foldl1' (M.<|>)) $ inners
      -- Note how `foldl1'` is a bit like `mconcat`.
      -- Count the number of serpents of various orientations. According to the
      -- problem specification, only one orientation should have serpents, so `sum`
      -- should give us that number of serpents.
      nSerpents       = sum $ map (countSerpents bigPicture) allSerpents
      -- The number of #'s in a serpent. Recall that hashes are ones.
      serpentHashes   = V.sum . M.getMatrixAsVector $ s1h
      -- The number of #'s in the big matrix.
      totalHashes     = V.sum . M.getMatrixAsVector $ bigPicture
  print $ totalHashes - (nSerpents * serpentHashes)


type Edge = V.Vector Int
data Tile = Tile
  { idnum                    :: Int
  , north, east, south, west :: Edge
  , inner                    :: M.Matrix Int
  }
  deriving (Eq, Show)

parseTile :: String -> Tile
parseTile t = Tile i n e s w x
 where
  (topLine : rest) = lines t
  rest01s          = map (map (\c -> if c == '#' then 1 else 0)) rest
  i                = read $ filter isDigit topLine
  n                = V.fromList $ head rest01s
  e                = V.fromList $ map last rest01s
  s                = V.fromList $ reverse (last rest01s)
  w                = V.fromList $ reverse (map head rest01s)
  x                = M.fromLists $ map (init . tail) (init . tail $ rest01s)

-- Return all possible edges of a tile.
getAllEdges :: Tile -> [Edge]
getAllEdges tile =
  [ f (g tile) | f <- [id, V.reverse], g <- [north, east, south, west] ]

-- Counts the number of edges of a tile that can match with any other edge in
-- a set of tiles. If this number is 2, then the tile is a corner tile. If
-- it's 3, then it's on the edge. If it's 4, then it's in the middle somewhere.
countMatchableEdges :: [Tile] -> Tile -> Int
countMatchableEdges allTiles tile = length . filter id $ searchHits
 where
  allOtherEdges :: [Edge]  -- All edges of other tiles.
  allOtherEdges =
    [ f t
    | f <- [north, south, east, west]
    , t <- allTiles
    , idnum t /= idnum tile
    ]
  thisTileEdges :: [Edge] -- The edges of the current tile, flipped in all ways.
  thisTileEdges =
    [ f (g tile) | f <- [id, V.reverse], g <- [north, south, east, west] ]
  searchHits :: [Bool]    -- Whether each edge was found in allOtherEdges.
  searchHits = map (`elem` allOtherEdges) thisTileEdges

-- Flip a matrix horizontally
flipH :: M.Matrix a -> M.Matrix a
flipH m =
  let c = M.ncols m
  in  foldl' (\f x -> f . M.switchCols x (c + 1 - x)) id [1 .. c `div` 2] m
-- Flip a matrix vertically
flipV :: M.Matrix a -> M.Matrix a
flipV m =
  let r = M.nrows m
  in  foldl' (\f x -> f . M.switchRows x (r + 1 - x)) id [1 .. r `div` 2] m
-- Flip a tile horizontally
tflipH :: Tile -> Tile
tflipH (Tile i n e s w x) = Tile i (r n) (r w) (r s) (r e) (flipH x)
  where r = V.reverse
-- Flip a tile vertically
tflipV :: Tile -> Tile
tflipV (Tile i n e s w x) = Tile i (r s) (r e) (r n) (r w) (flipV x)
  where r = V.reverse
-- Rotate a tile by 90 degrees clockwise.
trotate :: Tile -> Tile
trotate (Tile i n e s w x) = Tile i w n e s (flipH . M.transpose $ x)

-- Given a list of all tiles and the (already correctly oriented) leftmost tile
-- in a row, find the series of appropriately rotated tiles that make up the row.
-- The algorithm assumes that there is only one unique way to do this.
solveRow :: [Tile] -> Tile -> [Tile]
solveRow allTiles leftmost = map fromJust
                             . takeWhile isJust
                             . iterate (getNextTile . fromJust)
                             $ Just leftmost
 where
  getNextTile :: Tile -> Maybe Tile
  getNextTile leftTile = case rightTiles of
    []  -> Nothing  -- No matches found, i.e. we reached the end of the row.
    [x] -> Just $ orientT edgeToMatch x
    _   -> error "uh oh: solveRow"  -- We assume there is only one match.
   where
    otherTiles  = [ t | t <- allTiles, idnum t /= idnum leftTile ]
    edgeToMatch = east leftTile
    rightTiles  = filter (matchT edgeToMatch) otherTiles
  -- `matchT edge tile` checks whether the given tile can possibly be
  -- oriented in such a way that matches the given edge.
  matchT :: Edge -> Tile -> Bool
  matchT edge tile = edge `elem` getAllEdges tile
  -- Assuming we have found a tile that goes on the *right* of a target
  -- edge, `orientT target tile` will return tile correctly oriented. We
  -- need to carefully check which edge of the tile matches the target
  -- edge, and flip and/or rotate the tile appropriately based on that.
  orientT :: Edge -> Tile -> Tile
  orientT edge tile
    | edge == V.reverse (west tile)  = tile
    | edge == V.reverse (south tile) = trotate tile
    | edge == V.reverse (east tile)  = trotate . trotate $ tile
    | edge == V.reverse (north tile) = trotate . trotate . trotate $ tile
    | edge == west tile              = tflipV tile
    | edge == south tile             = trotate . tflipH $ tile
    | edge == east tile              = tflipH tile
    | edge == north tile             = tflipH . trotate $ tile
    | otherwise                        = error "uh oh: orientT in solveRow"
    -- There *has* to be a match, given that the tile we feed to orientT was
    -- obtained by searching for a match. If there isn't, something is wrong.


-- This code is the same as solveRow except that the directions are changed.
-- I don't particularly like that the code repeats itself, but I think this
-- is genuinely the simplest way to do it rather than trying to combine
-- it into one big function with lots of if/elses.
solveColumn :: [Tile] -> Tile -> [Tile]
solveColumn allTiles topmost = map fromJust
                               . takeWhile isJust
                               . iterate (getNextTile . fromJust)
                               $ Just topmost
 where
  getNextTile :: Tile -> Maybe Tile
  getNextTile topTile = case bottomTiles of
    []  -> Nothing
    [x] -> Just $ orientT edgeToMatch x
    _   -> error "uh oh: solveColumn"
   where
    otherTiles  = [ t | t <- allTiles, idnum t /= idnum topTile ]
    edgeToMatch = south topTile
    bottomTiles = filter (matchT edgeToMatch) otherTiles
  matchT :: Edge -> Tile -> Bool
  matchT edge tile = edge `elem` getAllEdges tile
  orientT :: Edge -> Tile -> Tile
  orientT edge tile
    | edge == V.reverse (north tile) = tile
    | edge == V.reverse (west tile)  = trotate tile
    | edge == V.reverse (south tile) = trotate . trotate $ tile
    | edge == V.reverse (east tile)  = trotate . trotate . trotate $ tile
    | edge == north tile             = tflipH tile
    | edge == west tile              = tflipH . trotate $ tile
    | edge == south tile             = tflipV tile
    | edge == east tile              = trotate . tflipH $ tile
    | otherwise                        = error "uh oh: orientT in solveColumn"


-- Orient the top-left corner such that the edges that match other tiles
-- are facing the east and south.
orientTopLeftCorner :: [Tile] -> Tile -> Tile
orientTopLeftCorner allTiles tile = tile'
 where
  allOtherEdges :: [Edge]
  allOtherEdges =
    [ f (g t)
    | f <- [id, V.reverse]
    , g <- [north, south, east, west]
    , t <- allTiles
    , idnum t /= idnum tile
    ]
  -- These show which edges can match with other tiles, i.e. are inside.
  nIn, eIn, sIn, wIn :: Bool
  [nIn, eIn, sIn, wIn] =
    map (\f -> f tile `elem` allOtherEdges) [north, east, south, west]
  tile' | sIn && eIn = tile
        | wIn && sIn = trotate . trotate . trotate $ tile
        | nIn && wIn = trotate . trotate $ tile
        | eIn && nIn = trotate tile
        | otherwise  = error "uh oh: orientTopLeftCorner"


-- Serpents of various orientations.
serpentFlat :: String
serpentFlat =
  "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   "
serpent :: [[Int]]
serpent = map (map (\c -> if c == '#' then 1 else 0)) . lines $ serpentFlat
s1h, s2h, s3h, s4h, s1v, s2v, s3v, s4v :: M.Matrix Int
s1h = M.fromLists serpent  -- right side up
s2h = flipV s1h            -- vertically flipped
s3h = flipH s1h            -- horizontally flipped
s4h = flipV . flipH $ s1h  -- rotated 180 degrees
s1v = M.transpose s1h      -- The vertical serpents. Same thing.
s2v = flipV s1v
s3v = flipH s1v
s4v = flipV . flipH $ s1v
allSerpents :: [M.Matrix Int]
allSerpents = [s1h, s2h, s3h, s4h, s1v, s2v, s3v, s4v]


-- Count the number of occurrences of a given serpent in the bigMatrix.
countSerpents :: M.Matrix Int -> M.Matrix Int -> Int
countSerpents bigMatrix serp = nSerpents
 where
  (sr, sc)  = (M.nrows serp, M.ncols serp)
  (mr, mc)  = (M.nrows bigMatrix, M.ncols bigMatrix)
  validRows = [1 .. mr - sr + 1]
  validCols = [1 .. mc - sc + 1]
  submatrices =
    [ M.submatrix p (p + sr - 1) q (q + sc - 1) bigMatrix
    | p <- validRows
    , q <- validCols
    ]
  containsSerpent :: M.Matrix Int -> Bool
  containsSerpent m = M.elementwiseUnsafe (*) m serp == serp
  nSerpents = length $ filter containsSerpent submatrices
