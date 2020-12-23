import           Data.List.Split                ( splitOn )
import           Data.IntMap                    ( IntMap )
import qualified Data.IntMap                   as IM
import           Paths_aoc20_hs                 ( getDataFileName )

main :: IO ()
main = do
  fname <- getDataFileName "d22.txt"
  input <- readFile fname
  let (deck1, deck2) = parseInput input

  putStr "Part 1: "
  print $ fst . runGame $ (deck1, deck2)
  putStr "Part 2: "
  print $ fst (runRecursiveGame (deck1, deck2) IM.empty)

type Deck = [Int]

parseInput :: String -> (Deck, Deck)
parseInput t = (deck1, deck2)
 where
  [_ : c1, _ : c2] = map lines $ splitOn "\n\n" t
  deck1            = map read c1
  deck2            = map read c2

score :: Deck -> Int
score deck = sum $ zipWith (*) [1 ..] (reverse deck)

-- The second returned value is a flag to indicate which player won. It is
-- True if player 1 won and False if player 2 won.
runGame :: (Deck, Deck) -> (Int, Bool)
runGame (deck1, deck2) = case (null deck1, null deck2) of
  (True , True ) -> error "uh oh"
  (True , False) -> (score deck2, False)
  (False, True ) -> (score deck1, True)
  (False, False) -> runGame $ update (deck1, deck2)

update :: (Deck, Deck) -> (Deck, Deck)
update (deck1, deck2) = (deck1', deck2')
 where
  left1:rest1      = deck1
  left2:rest2      = deck2
  (deck1', deck2') = if left1 > left2
    then (rest1 ++ [left1, left2], rest2)
    else (rest1, rest2 ++ [left2, left1])

-- The IntMap is indexed by the size of the first deck. The values are the concatenated decks
-- that have been seen before.
runRecursiveGame :: (Deck, Deck) -> IntMap [Deck] -> (Int, Bool)
runRecursiveGame (deck1, deck2) m = result
  where
    length1 = length deck1
    seenBefore = case m IM.!? length1 of 
                      Nothing -> False
                      Just x  -> (deck1 ++ deck2) `elem` x
    m'     = IM.insertWith (++) length1 [deck1 ++ deck2] m
    result = if seenBefore
                then (score deck1, True)
                else case (null deck1, null deck2) of
                          (True, True) -> error "uh oh"
                          (True, False) -> (score deck2, False)
                          (False, True) -> (score deck1, True)
                          (False, False) -> runRecursiveGame (recursiveUpdate (deck1, deck2)) m'

recursiveUpdate :: (Deck, Deck) -> (Deck, Deck)
recursiveUpdate (deck1, deck2) = (deck1', deck2')
 where
  left1:rest1      = deck1
  left2:rest2      = deck2
  recursiveNeeded  = left1 <= length rest1 && left2 <= length rest2
  player1WinsRound = if recursiveNeeded
                        then snd $ runRecursiveGame (take left1 rest1, take left2 rest2) IM.empty
                        else left1 > left2
  (deck1', deck2') = if player1WinsRound
                        then (rest1 ++ [left1, left2], rest2)
                        else (rest1, rest2 ++ [left2, left1])
