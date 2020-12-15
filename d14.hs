import           Data.Bits
import           Data.Int                    (Int64)
import           Data.Map                    (Map)
import qualified Data.Map             as      M
import           Data.List                   (foldl')
import           Data.Maybe                  (fromMaybe)
import           Control.Monad.State
import           Text.Megaparsec      hiding (State)
import           Text.Megaparsec.Char
import           Paths_aoc20_hs

type Addr   = Int64
type Val    = Int64
type Memory = Map Addr Val
-- The mask is stored as (zeros, ones, xs): each of these is a list containing
-- the bits that are zeros, ones, and xs respectively. The bits are
-- zero-indexed, following Data.Bits. So, for example, the mask 010101XX would
-- be represented as ([3, 5, 7], [2, 4, 6], [0, 1]).
type Mask   = ([Int], [Int], [Int])
-- Instead of passing the Memory as part of the state, I chose to make only the
-- mask be the state. The output from running each state processor is then an
-- association list of (Addr, Val), which can then be converted into a Memory.
type S      = State Mask [(Addr, Val)]

main :: IO ()
main = do
    fname <- getDataFileName "d14.txt"
    input <- readFile fname
    let instructions = map parseLine $ lines input

    putStr "Part 1: "
    let memory1 = runStateUsing mkState1 instructions
    print $ sum (M.elems memory1)

    putStr "Part 2: "
    let memory2 = runStateUsing mkState2 instructions
    print $ sum (M.elems memory2)


-- Helper function.
runStateUsing :: (Instruction -> S)  -- Function which turns instructions to state processors
              -> [Instruction]       -- List of instructions to run
              -> Memory              -- Final state of the memory
runStateUsing mkState instructions = memory
          -- Create a series of state processors.
    where stateFns = map mkState instructions
          -- The state processors can be combined using `sequence` to form one
          -- large state processor which runs all the state processors in
          -- order. We're not interested in the final state (i.e. the final
          -- mask), so we use `evalState` which throws that away and just
          -- collects the outputs.
          -- As long as the first instruction is to set a mask, we can even get
          -- away with using `undefined` as our initial state. Because of lazy
          -- evaluation, it will never be used for anything.
          collectedValues = evalState (sequence stateFns) undefined
          -- The state of the memory at the end can be determined very simply
          -- by concatenating the lists of outputs. Conveniently, `fromList`
          -- takes the *later* values if two values are present in the list
          -- twice, which is exactly what we want, as the memory address stores
          -- the most recent value.
          memory = M.fromList . concat $ collectedValues

-- Parsing
data Instruction = SetMask Mask
                 | SetMem Addr Val
                 deriving (Eq, Show)
type Parser = Parsec String String
parseLine :: String -> Instruction
parseLine = fromMaybe (error "bad input") . parseMaybe (parseSetMask <|> parseSetMem)
parseSetMask :: Parser Instruction
parseSetMask = do
    _ <- string "mask = "
    mask <- takeRest
    let zipped = zip (reverse [0..35]) mask
        zeros = map fst . filter ((=='0') . snd) $ zipped
        ones = map fst . filter ((=='1') . snd) $ zipped
        xs = map fst . filter ((=='X') . snd) $ zipped
    return $ SetMask (zeros, ones, xs)
parseSetMem :: Parser Instruction
parseSetMem = do
    _ <- string "mem["
    addr <- some digitChar
    _ <- string "] = "
    val <- some digitChar
    return $ SetMem (read addr) (read val)

-- Part 1
mkState1 :: Instruction -> S
mkState1 ins = state (f ins)
    where f (SetMask newMask) = const ([], newMask)
          f (SetMem addr val) = \mask -> ([(addr, applyMask1 mask val)], mask)
-- Apply the bit mask to the value.
applyMask1 :: Mask -> Val -> Val
applyMask1 (zeros, ones, _) val = let val2 = foldl' clearBit val zeros
                                      in foldl' setBit val2 ones

-- Part 2
mkState2 :: Instruction -> S
mkState2 ins = state (f ins)
    where f (SetMask newMask) = const ([], newMask)
          f (SetMem addr val) = \mask -> ([(addr', val) | addr' <- applyMask2 mask addr], mask)
-- Generate all possible memory addresses that will be written to after application
-- of a mask to an address.
-- The foldl' bit applies the ones, and the go function applies the floating bits.
applyMask2 :: Mask -> Addr -> [Addr]
applyMask2 (_, ones, xs) addr = go xs (foldl' setBit addr ones)
    where go :: [Int] -> Addr -> [Addr]
          go []     a = [a]
          go (x:xs) a = do
              a' <- go xs a
              [a' `setBit` x, a' `clearBit` x]
