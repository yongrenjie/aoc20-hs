import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Maybe (isJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d8.txt"
    input <- readFile fname
    let instructions = IntMap.fromList . zip [1..] .  map parseLine . lines $ input
    putStr "Part 1: "
    print $ runProgram instructions initialState
    putStr "Part 2: "
    let sz = IntMap.size instructions
    let (Just allChanged) = sequence . filter isJust $ twiddle <$> [1..sz] <*> [instructions]
    print $ filter isTerminated (runProgram <$> allChanged <*> [initialState])

-- Parsing
data Instruction = Acc Int
                 | Nop Int
                 | Jmp Int
                 deriving Show

type Parser = Parsec String String

parseLine :: String -> Instruction
parseLine line = case runParser parser "" line of
                      Left l -> error (show l)
                      Right r -> r
    where parser :: Parser Instruction
          parser = parseType <* space <*> parseNumber'
          parseType :: Parser (Int -> Instruction)
          parseType = choice [Acc <$ string "acc",
                              Nop <$ string "nop",
                              Jmp <$ string "jmp"]
          parseNumber :: Parser Int
          parseNumber = do
              c <- char '+' <|> char '-'
              n <- some digitChar
              return $ read n * (if c == '+' then 1 else -1)
          -- seen online
          parseNumber' :: Parser Int
          parseNumber' = (id <$ char '+' <|> negate <$ char '-')
                             <*> (read <$> some digitChar)

-- Accumulator, current position, and set of visited positions
data ProgramState = Running (Int, Int, IntSet)
                  | InfiniteLoop (Int, Int, IntSet)
                  | Terminated (Int, Int, IntSet)
                  | OutOfBounds (Int, Int, IntSet)
                  deriving Show

initialState :: ProgramState
initialState = Running (0, 1, IntSet.empty)

-- Runs the single instruction at the index referred to by the current position
-- of the program state.
runProgram :: IntMap Instruction -> ProgramState -> ProgramState
runProgram inst state = runProg' state
    where sz = IntMap.size inst
          runProg' (Running s@(acc, pos, visited)) =
              case (IntSet.member pos visited, IntMap.lookup pos inst) of
                   -- (already visited this position, position doesn't exist)
                   (True, _)    -> InfiniteLoop s
                   (_, Nothing) -> if pos == sz + 1 then Terminated s else OutOfBounds s
                   (_, Just i)  -> runProgram inst $ Running (acc', pos', visited')
                                   where acc' = case i of
                                                     Acc n -> acc + n
                                                     _ -> acc
                                         pos' = case i of
                                                     Jmp n -> pos + n
                                                     _ -> pos + 1
                                         visited' = IntSet.insert pos visited
          runProg' anyOther = anyOther

-- Changes position i of a list of instructions from Nop to Jmp or Jmp to Nop.
-- If the instruction at position i is Acc (or if it's out of bounds), returns
-- Nothing.
twiddle :: Int -> IntMap Instruction -> Maybe (IntMap Instruction)
twiddle i m = case IntMap.lookup i m of
                   Nothing -> Nothing
                   Just (Acc _) -> Nothing
                   Just (Jmp n) -> Just $ IntMap.adjust (const $ Nop n) i m
                   Just (Nop n) -> Just $ IntMap.adjust (const $ Jmp n) i m

isTerminated :: ProgramState -> Bool
isTerminated (Terminated _) = True
isTerminated _ = False
