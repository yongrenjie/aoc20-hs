import Data.List.Split
import qualified Data.Map as M
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d7.txt"
    input <- readFile fname
    let rulebook = parseInput input
    putStr $ "Part 1: "
    print $ countValidTopLevel "shiny gold" rulebook
    putStr $ "Part 2: "
    print $ (countTotalContained "shiny gold" rulebook) - 1
    -- countTotalContained gives a number which includes itself.

type Rule = (String, [(Integer, String)])
type Rulebook = M.Map String [(Integer, String)]

parseInput :: String -> Rulebook
parseInput = M.fromList . map getRule . lines

getRule :: String -> Rule
getRule cs = (baseColor, validContents)
    where baseColor = unwords . take 2 . words $ cs
          restOfRule = unwords . drop 4 . words $ cs
          validContents = if restOfRule == "no other bags."
                             then []
                             else parse restOfRule
          parse :: String -> [(Integer, String)]
          parse = map (\r -> (num r, color r)) . splitOn ","
              where num :: String -> Integer
                    num = read . head . words
                    color :: String -> String
                    color = unwords . take 2 . drop 1 . words

isContained :: Rulebook -> String -> String -> Bool
isContained rb child parent = child `isContained2` parent
    where isContained2 :: String -> String -> Bool
          child2 `isContained2` parent2 =
              if child2 == parent2 then True
                                   else any (child2 `isContained2`) (containedColors parent2)
          -- Because a bag of a given colour cannot actually contain itself,
          -- we have to remember to subtract 1 later on.
          containedColors :: String -> [String]
          containedColors clr = case M.lookup clr rb of
                                     Nothing -> []
                                     Just x -> map snd x
          -- The Nothing branch should never trigger, unless our input is wrong.

countValidTopLevel :: String -> Rulebook -> Int
countValidTopLevel clr rb = (length . filter (isContained rb clr) $ M.keys rb) - 1

countTotalContained :: String -> Rulebook -> Integer
countTotalContained clr rb =
    case M.lookup clr rb of
         Nothing -> 0
         Just [] -> 1
         Just contents -> 1 + (sum . map f $ contents)
    where f :: (Integer, String) -> Integer
          f = \(n, c) -> n * countTotalContained c rb
