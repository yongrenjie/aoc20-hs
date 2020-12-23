{-# LANGUAGE ForeignFunctionInterface #-}

import           Data.Char                      ( isSpace
                                                , ord
                                                )
import           Data.List                      ( elemIndex )
import           Data.Maybe                     ( fromJust )
import           Foreign                        ( )
import           Foreign.C.Types
import           Paths_aoc20_hs                 ( getDataFileName )

foreign import ccall "d23b.h aoc_d23_part2"
  c_runPart2 :: CLong

main :: IO ()
main = do
  fname <- getDataFileName "d23.txt"
  input <- readFile fname
  let initialState = map (\c -> ord c - ord '0') . filter (not . isSpace) $ input

  putStr "Part 1: "
  putStrLn $ getOrder . (!! 100) $ iterate update initialState

  putStr "Part 2: "
  print (fromIntegral c_runPart2 :: Integer)

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
