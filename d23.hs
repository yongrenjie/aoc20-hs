{-# LANGUAGE ForeignFunctionInterface #-}

import           Data.Char                      ( isSpace
                                                , ord
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import qualified Data.Vector.Storable          as SV
import           Foreign                        ( )
import           Foreign.Ptr
import           Foreign.C.Types
import           System.IO.Unsafe               ( unsafePerformIO )
import           Paths_aoc20_hs                 ( getDataFileName )

main :: IO ()
main = do
  fname <- getDataFileName "d23.txt"
  input <- readFile fname
  let initialState = map (\c -> ord c - ord '0') . filter (not . isSpace) $ input

  putStr "Part 1 (with Data.Sequence): "
  putStrLn $ getOrder . (!! 100) $ iterate update initialState

  -- runGame doesn't return the entire game state, only the product of the two
  -- cups after cup 1 (i.e. the requested solution for Part 2).
  putStr "Part 1 product of two cups after cup 1 (with FFI): "
  print $ runGame initialState 0 100

  putStr "Part 2 (with FFI): "
  print $ runGame initialState 1000000 10000000

update :: [Int] -> [Int]
update cups = cups'
  where
    currentCup:rest = cups
    (pickedUpCups, otherCups) = splitAt 3 rest
    destinationCup = case filter (< currentCup) otherCups of
      [] -> maximum otherCups
      xs -> maximum xs
    destinationIndex = fromJust . elemIndex destinationCup $ otherCups
    (other1, other2) = splitAt (destinationIndex + 1) otherCups
    -- We place the currentCup at the back so that the new head of the list
    -- will be the currentCup for the next round.
    cups' = mconcat [other1, pickedUpCups, other2, [currentCup]]

getOrder :: [Int] -> String
getOrder cups = ans
  where
    index1 = fromJust $ elemIndex 1 cups
    (before1, _ : after1) = splitAt index1 cups
    ans = concatMap show (after1 ++ before1)

-- The C function signature:
-- long int run_game(int *first_input, int first_input_len, int full_len, int nrounds);
foreign import ccall unsafe "d23.h run_game"
  c_runGame :: Ptr CInt      -- The initial values specified in the puzzle input.
               -> CInt       -- The length of those initial values.
               -> CInt       -- The value to pad up until. Must be at least equal to
                             -- the length of initial values. This is not checked in C
                             -- code, but the Haskell wrapper does check.
               -> CInt       -- The number of rounds to play.
               -> IO CLong   -- The final score.

-- And the Haskell function which uses it:
runGame :: [Int]      -- The initial values. We don't need to provide a length.
           -> Int     -- The number to pad up until.
           -> Int     -- The number of rounds to play.
           -> Integer -- The final score.
runGame initial padding rounds = score
  where
    initialSV = SV.fromList . map fromIntegral $ initial
    len = SV.length initialSV
    len' = fromIntegral len
    padding' = fromIntegral (max padding len) -- make sure that padding' >= len.
    rounds' = fromIntegral rounds
    score = fromIntegral . unsafePerformIO $
      SV.unsafeWith initialSV (\ptr -> c_runGame ptr len' padding' rounds')
