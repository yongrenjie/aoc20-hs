import Data.List.Split
import qualified Data.Map as M
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d7.txt"
    input <- readFile fname
    let rulebook = parseInput input
    -- Both of these functions give one extra value (i.e. the requested color
    -- itself). However, a shiny gold bag cannot contain itself, so we need to
    -- subtract one both times.
    putStr "Part 1: "
    print $ countValidTopLevel "shiny gold" rulebook - 1
    putStr "Part 2: "
    print $ countTotalContained "shiny gold" rulebook - 1

type Rule = (String, [(Integer, String)])
type Rulebook = M.Map String [(Integer, String)]

-- Parsing.
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

-- isContained checks whether a bag of color _parent_ can contain a bag of color
-- _child_, according to a rulebook _rb_.
isContained :: Rulebook -> String -> String -> Bool
isContained rb child parent = child `isContained2` parent
    -- The child is contained in the parent either if it is the parent, or if one of
    -- the parent's containedColors contains the child.
    where isContained2 :: String -> String -> Bool
          child2 `isContained2` parent2 = (child2 == parent2)
              || any (child2 `isContained2`) (containedColors parent2)
          containedColors :: String -> [String]
          containedColors clr = map snd $ M.findWithDefault [] clr rb

countValidTopLevel :: String -> Rulebook -> Int
countValidTopLevel clr rb = length . filter (isContained rb clr) $ M.keys rb

countTotalContained :: String -> Rulebook -> Integer
countTotalContained clr rb =
    case M.lookup clr rb of
         Nothing -> 0
         Just contents -> 1 + (sum . map f $ contents)
    where f :: (Integer, String) -> Integer
          -- The total number of bags in n bags with colour c.
          f = \(n, c) -> n * countTotalContained c rb
