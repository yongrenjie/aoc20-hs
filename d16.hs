import Control.Monad (guard)
import Data.List (delete, transpose, (\\), isPrefixOf)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Map.Strict as M
import Paths_aoc20_hs ( getDataFileName )

main :: IO ()
main = do
    fname <- getDataFileName "d16.txt"
    input <- readFile fname
    let (rb, tickets) = parseInput input

    putStr "Part 1: "
    let allNumbers = concat tickets
        allInvalidNumbers = filter (`isInvalidValueForAllFields` rb) allNumbers
    print $ sum allInvalidNumbers

    putStr "Part 2: "
    let validTickets = filter (isValidTicket rb) tickets
        sortedFields = solve3 rb validTickets
        departureIndices = map snd . filter (("departure" `isPrefixOf`) . fst) $ zip sortedFields [0..]
        myTicket = head validTickets
    print $ product (map (myTicket !!) departureIndices)


-- Parsing. Not the cleanest, but it'll do.
type Rulebook = Map String (Int, Int, Int, Int)
type Ticket = [Int]
parseInput :: String -> (Rulebook, [Ticket])
parseInput input = (rulebook, tickets) where
    (ruleLines:yourTicket:nearbyTickets:_) = splitOn "\n\n" input
    rulebook = parseRulebook ruleLines
    tickets = parseTickets yourTicket nearbyTickets
type Parser = Parsec String String
parseRulebook :: String -> Rulebook
parseRulebook rb = M.fromList $ map (fromMaybe (error "bad input") . parseMaybe p) (lines rb) where
    p :: Parser (String, (Int, Int, Int, Int))
    p = do
        name <- some $ anySingleBut ':'
        _ <- string ": "
        int1 <- some digitChar
        _ <- char '-'
        int2 <- some digitChar
        _ <- string " or "
        int3 <- some digitChar
        _ <- char '-'
        int4 <- some digitChar
        return (name, (read int1, read int2, read int3, read int4))
parseTickets :: String -> String -> [Ticket]
parseTickets my yours = map lineToTicket . filter ((`elem` "1234567890") . head) $ ticketLines where
    ticketLines = lines my ++ lines yours
    lineToTicket :: String -> Ticket
    lineToTicket = map read . splitOn ","

-- Part 1
isValidValue :: Int -> (Int, Int, Int, Int) -> Bool 
isValidValue x (min1, max1, min2, max2) = (x >= min1 && x <= max1) || (x >= min2 && x <= max2)
isInvalidValue :: Int -> (Int, Int, Int, Int) -> Bool
isInvalidValue x t = not (isValidValue x t)
-- Checks if an integer does not satisfy all of the rules in the rulebook.
isInvalidValueForAllFields :: Int -> Rulebook -> Bool
isInvalidValueForAllFields x rb = all (isInvalidValue x) (M.elems rb)
-- Checks if an integer satisfies any of the rules in the rulebook.
isValidValueForAnyField :: Int -> Rulebook -> Bool
isValidValueForAnyField x rb = any (isValidValue x) (M.elems rb)

-- Part 2
isValidTicket :: Rulebook -> Ticket -> Bool
isValidTicket rb ts = all (`isValidValueForAnyField` rb) ts

-- Exponential scaling, works on sample input but too slow on real input
solve :: [String] -> Rulebook -> Int -> [Ticket] -> [[String]]
solve [] _ _ _  = return []
solve fs rb i ts = do
    field <- fs
    let values = map (!! i) ts
        constraint = rb M.! field
    guard $ all (`isValidValue` constraint) values
    otherFields <- solve (delete field fs) rb (i + 1) ts
    return $ field : otherFields

-- Dynamic programming ... Still too slow!!!
type Cache = Map String (Map Int Bool)

lookupCache :: Cache -> String -> Int -> Bool
lookupCache c s i = (c M.! s) M.! i

populateCache :: [Ticket] -> Rulebook -> Cache
populateCache ts rb = M.fromList $ zip fields (map mkSubmap fields) where
    fields = M.keys rb
    vals = transpose ts
    indices = [0..(length vals - 1)]
    mkSubmap :: String -> Map Int Bool
    mkSubmap f = M.fromList $ zip indices (map (checkValid f) indices) where
        getConstraint :: String -> (Int, Int, Int, Int)
        getConstraint field = rb M.! field
        checkValid :: String -> Int -> Bool
        checkValid field index = all (`isValidValue` getConstraint field) (vals !! index)

solve' :: Bool -> [String] -> Int -> Cache -> [[String]]
solve' False _ _ _ = []
solve' True [] _ _ = return []
solve' True fs i cache = do
    field <- fs
    let ok = lookupCache cache field i
    otherFields <- solve' ok (delete field fs) (i + 1) cache
    guard ok
    return $ field : otherFields


-- Third time lucky, looking up solutions online, apparently there is a pattern
-- in that there will be one index which only allows one attribute. This allows
-- us to remove it from the remaining lists (rather like solving Sudoku).
-- Repeatedly iterating this will yield the solution quickly.
-- Largely stolen from https://github.com/haskelling/aoc2020/blob/main/16b.hs
solve3 :: Rulebook -> [Ticket] -> [String]
solve3 rb ts = map head $ runUntilUnchanged removeKnowns validFields
  where
    allFields = M.keys rb
    vals = transpose ts
    validFields = map getValidFields vals

    isValidField :: [Int] -> String -> Bool
    isValidField vs f = all (`isValidValue` (rb M.! f)) vs

    getValidFields :: [Int] -> [String]
    getValidFields vs = filter (isValidField vs) allFields

    getKnowns :: [[String]] -> [String]
    getKnowns = map head . filter ((== 1) . length)

    removeKnowns :: [[String]] -> [[String]]
    removeKnowns fss = map (trimKnowns (getKnowns fss)) fss where
        trimKnowns :: [String] -> [String] -> [String]
        trimKnowns knownf allf = case length allf of
                                      1 -> allf
                                      _ -> allf \\ knownf
    
    runUntilUnchanged :: Eq a => (a -> a) -> a -> a
    runUntilUnchanged f x = let x' = f x in if x' == x then x else runUntilUnchanged f x'