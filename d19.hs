import qualified Data.IntMap                   as IM
import           Data.IntMap                    ( IntMap )
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( isJust, fromJust )
import           Debug.Trace                    ( trace )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.List.Split                ( splitOn )
import           Paths_aoc20_hs

main :: IO ()
main = do
  fname <- getDataFileName "d19.txt"
  input <- readFile fname
  let [rules, messages] = map lines $ splitOn "\n\n" input

  putStr "Part 1: "
  let parser0   = getParser' rules 0
      successes = map (parseMessage parser0) messages
  print $ length (filter id successes)

  putStr "Part 2: "
  putStrLn "Not done"


-- Step 1: Parse the rules given as strings and return parsers
type Parser = Parsec () String

extractRuleNumber :: String -> Int
extractRuleNumber = read . head . splitOn ":"

getParser' :: [String] -> Int -> Parser ()
getParser' rules n = p n
 where
  table = IM.fromList $ map (parseRule rules) rules
  p     = (table IM.!)

parseRule :: [String] -> String -> (Int, Parser ())
parseRule rules = fromJust . parseMaybe (pRuleToParser rules)

pRuleToParser :: [String] -> Parser (Int, Parser ())
pRuleToParser rules = do
  ruleNumber <- read <$> some digitChar
  _          <- string ": "
  ruleParser <- try (pOrRule rules) <|> try (pAndRule rules) <|> (pCharRule rules)
  -- From these traced values it is obvious that the same rule is being parsed
  -- multiple times (in total, 198696 values get traced!!!)
  return $ show ruleNumber `trace` (ruleNumber, ruleParser)

pOrRule :: [String] -> Parser (Parser ())
pOrRule rules = do
  leftParser  <- pAndRule rules
  _           <- string "| "
  rightParser <- pAndRule rules
  return $ (try leftParser <|> rightParser) >> return ()
pAndRule :: [String] -> Parser (Parser ())
pAndRule rules = do
  ruleNumbers <- some $ (read <$> some digitChar) <* space
  return $ mapM_ (getParser' rules) ruleNumbers
pCharRule :: [String] -> Parser (Parser ())
pCharRule _ = do
  _ <- char '"'
  c <- anySingle
  _ <- char '"'
  return $ char c >> return ()

-- Then run the parsers on the given inputs
parseMessage :: Parser () -> String -> Bool
parseMessage parser message = isJust $ parseMaybe parser message
