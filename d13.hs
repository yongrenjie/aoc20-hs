import           Data.List          (sortOn)
import           Data.List.Split    (splitOn)
import           Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d13.txt"
    (minTime, buses) <- parseInput <$> readFile fname
    let busIDs = map snd buses
    putStr "Part 1: "
    print $ uncurry (*) . head . sortOn snd . map (getWaitTime minTime) $ busIDs
    putStr "Part 2: "
    print $ buses
    print $ findSmallestN buses

parseInput :: String -> (Int, [(Int, Int)])
parseInput input = (minTime, buses)
    where [minTimeString, busString] = lines input
          minTime = read minTimeString
          buses = map (\(index, busID) -> (index, read busID))
                  . filter ((/= "x") . snd) 
                  . zip [0..]
                  . splitOn ","
                  $ busString

getWaitTime :: Int         -- minTime
            -> Int         -- the bus ID
            -> (Int, Int)  -- the time you have to wait
getWaitTime minTime busID = (busID, case minTime `rem` busID of
                                         0 -> 0
                                         x -> busID - x)

-- Chinese remainder theorem solver, from CLRS Chapter 31 pp 951-3
-- Note that I don't actually *know* what I'm doing. I'm just following
-- the book.
findSmallestN :: [(Int, Int)] -> Int
findSmallestN buses = ans
    where (times, ns) = unzip buses
          as = zipWith (-) ns times  -- ans + time must divide n, so ans === (n - time) (mod n)
          prod = product ns
          ms = map (prod `div`) ns
          cs = [m * (m `invMod` n) | (m, n) <- zip ms ns]
          ans = (sum $ zipWith (*) as cs) `mod` prod
          invMod :: Int -> Int -> Int
          -- m `invMod` n computes m^{-1} mod n, i.e. the value of x such
          -- that m * x == 1 (mod n).
          invMod m n = x where (_, x, _) = extendedEuclid m n

-- Extended Euclid from CLRS Chpt 937
-- extendedEuclid a b = (d, x, y)
-- where d = gcd(a, b) = ax + by
extendedEuclid :: Int -> Int -> (Int, Int, Int)
extendedEuclid a b = case b of
                          0 -> (a, 1, 0)
                          _ -> let (d', x', y') = extendedEuclid b (a `mod` b)
                                   in (d', y', x' - (a `div` b) * y')
