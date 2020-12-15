import qualified Data.IntMap.Strict   as      Map
import           Data.List.Split             (splitOn)
import           Control.Monad.State
import           Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d15.txt"
    input <- readFile fname
    let startingNumbers = (map read . splitOn "," $ input :: [Int])
        m = Map.fromList $ zip startingNumbers . map (:[]) $ [1..]
        t = last $ zip [1..] startingNumbers
        startingState = (m, t)
        newState = evalState (replicateM 30000000 update) startingState
    putStr "Part 1: "
    print $ lookup 2020 newState
    putStr "Part 2: "
    print $ lookup 30000000 newState

type Spoken = Int
type Turn   = Int
--           Map k       v
-- type M = Map.Map Spoken [Turn]
type M = Map.IntMap [Turn]
-- k: The keys represent the numbers that are spoken.
-- v: The values represent the turns at which they were spoken.
--    The later turns are placed at the front of the list.

--       State s                    a
type S = State (M, (Turn, Spoken)) (Turn, Spoken)
-- s: The state that is passed is a tuple of the map described above,
--    together with the output of the previous turn.
-- a: The output is a tuple of the number spoken, together with the
--    turn on which it was spoken.

update :: S
update = state f
    where f (m, (lastTurn, lastSpoken)) = ((nextTurn, nextSpoken), (m', (nextTurn, nextSpoken)))
              where nextSpoken = case Map.lookup lastSpoken m of
                                      Just [_]     -> 0
                                      Just (x:y:_) -> x - y
                                      _            -> error "yousa in big doo doo"
                    nextTurn = lastTurn + 1
                    m' = Map.insertWith (++) nextSpoken [nextTurn] m
