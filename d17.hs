import           Control.Monad                  ( guard )
import           Data.IntSet                    ( IntSet )
import qualified Data.IntSet                   as IS
import           Paths_aoc20_hs

{-
- Overall strategy
- ----------------
-
- It appears that nearly all of the board will be empty, so it doesn't make sense
- to use a list or a vector to store what is mostly empty space. In a way, we need
- a sparse 4D matrix... which seems like it can be appropriately represented by an
- IntSet.
-
- In our IntSet, Ints corresponding to active cells will be members of the set.
- This means that each position in the game board must be mapped to a unique Int
- to be stored in the set.
-
- Since there are only six game turns, if the initial input has a size of m*m*1*1,
- the final game state can at most have a size of (m+12)*(m+12)*(1+12)*(1+12). In
- practice, it is a bit less confusing to just use (m+12)*(m+12)*(m+12)*(m+12). So,
- we can map (x, y, z, w) to a unique Int through
-
-    (x, y, z, w) --> (x+6)*((m+12)^3) + (y+6)*((m+12)^2) + (z+6)*(m+12) + (w+6)
-
- where we add 6 to everything to avoid negative numbers. (This also makes the
- reverse conversion much easier as we can just `divMod` without fear.) This is
- handled by `ind` and `deind`.
-
- This is a strategy adopted from /u/tel's post at
-
-     https://www.reddit.com/r/haskell/comments/34kqer
-}

-- We'll hardcode this. I'm just way too lazy to pass it around all the time.
initialSize :: Int
initialSize = 8
nTurns :: Int
nTurns = 6

main :: IO ()
main = do
  fname <- getDataFileName "d17.txt"
  input <- readFile fname
  let s = initialise input
  putStr "Part 1: "
  print $ IS.size . head . drop nTurns $ iterate (update False) s
  putStr "Part 2: "
  print $ IS.size . head . drop nTurns $ iterate (update True) s

-- This code looks like it works for 4D, but in truth it works for 3D too as long
-- as we *only* consider positions with w = 0, i.e. one 3D hyperplane of the 4D surface.
initialise :: String -> IntSet
initialise input = IS.fromList $ do
  (y, line) <- zip [0 ..] (lines input)
  x         <- map fst . filter ((== '#') . snd) $ zip [0 ..] line
  return $ ind (x, y, 0, 0)

-- We need to shift everything by nTurns so that `divMod` deals with the negative
-- numbers correctly.
ind :: (Int, Int, Int, Int) -> Int
ind (x, y, z, w) =
  let m12 = initialSize + (2 * nTurns)
  in  ((((w + nTurns) * m12) + (z + nTurns)) * m12 + (y + nTurns)) * m12 + x + nTurns
  -- Horner's method.

deind :: Int -> (Int, Int, Int, Int)
deind n =
  let m12       = initialSize + (2 * nTurns)
      (myzw, x) = n `divMod` m12
      (zw  , y) = myzw `divMod` m12
      (w   , z) = zw `divMod` m12
  in  (x - nTurns, y - nTurns, z - nTurns, w - nTurns)

-- Get all cells that are adjacent in 3D or 4D, depending on which part we're solving.
getAdj :: Bool -> Int -> IntSet
getAdj isPart2 = IS.fromList . map ind . f . deind
 where
  f :: (Int, Int, Int, Int) -> [(Int, Int, Int, Int)]
  f (x, y, z, w) = do
    x' <- [x - 1 .. x + 1]
    y' <- [y - 1 .. y + 1]
    z' <- [z - 1 .. z + 1]
    w' <- if isPart2 then [w - 1 .. w + 1] else [w]
    guard $ (x', y', z', w') /= (x, y, z, w)
    -- Note that we do not need to check for out-of-bounds, because it is impossible
    -- that an out-of-bounds square will become active during the six game turns. Thus,
    -- an out-of-bounds square will never be in the IntSet game state (i.e. it is
    -- always inactive), which is equivalent to it not existing in the first place.
    -- If we ran the game for more than 6 turns we would have to enlarge the IntSet.
    return (x', y', z', w')

-- Count how many cells adjacent to cell i are active in the current game
-- getAdj i finds all the cells adjacent to i, and s is all the currently
-- active cells. So the intersection of s and (getAdj i) represents all 
-- the active cells adjacent to i.
countAdjActive :: Bool -> IntSet -> Int -> Int
countAdjActive isPart2 s i = IS.size $ IS.intersection s (getAdj isPart2 i)

-- The state machine.
update :: Bool -> IntSet -> IntSet
update isPart2 s = s'
 where
  activeCellsToCheck   = s
  inactiveCellsToCheck = (IS.foldr (IS.union . (getAdj isPart2)) IS.empty s) IS.\\ s
  activeCellsToFlip    = IS.filter (not . p . (countAdjActive isPart2 s)) activeCellsToCheck
  p :: Int -> Bool
  p i = i == 2 || i == 3
  inactiveCellsToFlip  = IS.filter ((== 3) . (countAdjActive isPart2 s)) inactiveCellsToCheck
  s' = (IS.union s inactiveCellsToFlip) IS.\\ (activeCellsToFlip)
