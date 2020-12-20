import           Data.List                      ( sortOn )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust, fromJust )
import           Debug.Trace                    ( trace )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Paths_aoc20_hs


main :: IO ()
main = do
  fname <- getDataFileName "d19.txt"
  input <- readFile fname
  let [rules, messages] = map lines $ splitOn "\n\n" input
      sortedRules = sortOn extractRuleNumber rules

  putStr "Part 1: "
  let parser0   = getParser' sortedRules 0
      successes = map (parseMessage parser0) messages
  print $ length (filter id successes)

  putStr "Part 2: "
  putStrLn "Not done"


-- Step 1: Parse the rules given as strings and return parsers
type Parser = Parsec () String

extractRuleNumber :: String -> Int
extractRuleNumber = read . head . splitOn ":"

getParser' :: [String] -> Int -> Parser ()
getParser' sortedRules = p
 where
  -- This is tantalisingly close to the typical memoisation model in Haskell,
  -- and is very close to 100%, as `pRuleToParser` is only being called 159 times
  -- (there are 136 rules in the puzzle input).
  --
  -- Previous iterations:
  --   - I wrote getParser' sortedRules n = p n (not eta-reduced) (1461 calls).
  --   - I used an IntMap instead of a plain list (198,696 calls).
  --   - I also tried it with a plain old association list. (938 calls)
  table = map (snd . parseRule sortedRules) sortedRules
  p     = (table !!)

parseRule :: [String] -> String -> (Int, Parser ())
parseRule rules = fromJust . parseMaybe (pRuleToParser rules)

pRuleToParser :: [String] -> Parser (Int, Parser ())
pRuleToParser srules = do
  ruleNumber <- read <$> some digitChar
  _          <- string ": "
  ruleParser <- try (pOrRule srules) <|> try (pAndRule srules) <|> (pCharRule srules)
  -- From these traced values we can tell that the same rule is being parsed
  -- multiple times.
  return $ show ruleNumber `trace` (ruleNumber, ruleParser)

pOrRule :: [String] -> Parser (Parser ())
pOrRule srules = do
  leftParser  <- pAndRule srules
  _           <- string "| "
  rightParser <- pAndRule srules
  return $ (try leftParser <|> rightParser) >> return ()
pAndRule :: [String] -> Parser (Parser ())
pAndRule srules = do
  ruleNumbers <- some $ (read <$> some digitChar) <* space
  return $ mapM_ (getParser' srules) ruleNumbers
pCharRule :: [String] -> Parser (Parser ())
pCharRule _ = do
  _ <- char '"'
  c <- anySingle
  _ <- char '"'
  return $ char c >> return ()

-- Run a parser on a given message.
parseMessage :: Parser () -> String -> Bool
parseMessage parser message = isJust $ parseMaybe parser message
