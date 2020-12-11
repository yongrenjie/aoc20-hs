import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d10.txt"
    input <- readFile fname
    let adaptors = sort . map (read :: String -> Int) . lines $ input
    let (diff1, _, diff3) = getJoltDiffs (0:adaptors)
    putStrLn $ "Part 1: " ++ show (diff1 * diff3)
    putStrLn $ "Part 2: " ++ show (fst $ countValidCombos2 0 IM.empty adaptors)

-- assumes input is sorted, also adds the 3-jolt difference for the device
-- itself.
getJoltDiffs :: [Int] -> (Int, Int, Int)
getJoltDiffs []  = (0, 0, 1)
getJoltDiffs [_] = (0, 0, 1)
getJoltDiffs (x:y:ys) = (p + dp, q + dq, r + dr)
    where (p, q, r) = getJoltDiffs (y:ys)
          (dp, dq, dr) = case y - x of
                              1 -> (1, 0, 0)
                              2 -> (0, 1, 0)
                              3 -> (0, 0, 1)
                              _ -> error "invalid input"

-- Brute force. Too slow!!
countValidCombos :: Int -> [Int] -> Int
countValidCombos _ [] = 1
countValidCombos _ [_] = 1
countValidCombos p [_,r] = if r - p <= 3 then 2 else 1
countValidCombos p (q:r:s:x) = cq + cr + cs
    where cq = if q - p <= 3 then countValidCombos q (r:s:x) else 0
          cr = if r - p <= 3 then countValidCombos r (s:x) else 0
          cs = if s - p <= 3 then countValidCombos s x else 0

-- Same as above but caching results
type Cache = IntMap Int
countValidCombos2 :: Int -> Cache -> [Int] -> (Int, Cache)
countValidCombos2 k c m = case IM.lookup k c of
                               Just x -> (x, c)
                               Nothing -> countValidCombos2b k c m
    where countValidCombos2b :: Int -> Cache -> [Int] -> (Int, Cache)
          countValidCombos2b k c [] = (1, IM.insert k 1 c)
          countValidCombos2b k c [_] = (1, IM.insert k 1 c)
          countValidCombos2b p c [_,r] = if r - p <= 3
                                            then (2, IM.insert p 2 c)
                                            else (1, IM.insert p 1 c)
          countValidCombos2b p c (q:r:s:x) = (val, IM.insert p val newCache)
              where val = cq + cr + cs
                    (cq, c1) = if q - p <= 3
                                  then countValidCombos2 q c (r:s:x)
                                  else (0, c)
                    (cr, c2) = if r - p <= 3
                                  then countValidCombos2 r c1 (s:x)
                                  else (0, c1)
                    (cs, c3) = if s - p <= 3
                                  then countValidCombos2 s c2 x 
                                  else (0, c2)
                    newCache = c3
