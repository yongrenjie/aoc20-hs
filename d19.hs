import           Control.Monad                  ( guard
                                                , void
                                                )
import           Data.List                      ( sortOn )
import           Data.List.Split                ( splitOn )
import           Data.Maybe                     ( isJust
                                                , fromJust
                                                )
-- import           Debug.Trace                    ( trace )
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Paths_aoc20_hs

{-
- Overall strategy
- ----------------
-
- Each rule should be turned into a parser of its own: when we apply this parser
- to a message, that will tell us whether the message fits the rules. Since we
- only care about whether the message is successfully parsed, the return type
- of the parser can simply be ().
-
- Let's say we have a function, called `getParser`, which takes a rule number and
- returns the appropriate parser. The problem is that this function is recursive:
- for example, the result of parsing rule 0 (using `getParser 0`) should give us
- something like
-
-     getParser :: Int -> Parser ()
-     getParser 0 = return $ (getParser 8) >> (getParser 11) >> return ()
-     -- Note that this line shows the intended result, not the implementation.
-
- We could recursively call getParser until we reach the bottom single-character
- rules, but that would be terribly inefficient. It would be better if we could
- memoise the results of `getParser` somehow.
-
- To do this, we *tabulate* the results of getParser. That is, we define a kernel
- function `getParserK` for which `getParser` is its fixed point:
-
-     getParserK fp = fp   ===>   fp === getParser  ................. (1)
-
- and then we can get the fixed point by
- 
-     getParser = tabulate getParserK               ................. (2)
-
- This is adapted from the Fibonacci memoisation described in the Oxford CS notes
- https://www.cs.ox.ac.uk/teaching/materials20-21/fp/lecture14.pdf.
-
- The only difference here is that the parser function depends on the input. For
- example, imagine if we had to memoise a Fibonacci-type function for which `fib 0`
- and `fib 1` were not hardcoded as 0 and 1 respectively, but were instead specified
- in a file. To get around this, we introduce extra function parameters where necessary
- so that the (sorted) rules can be passed around between `tabulate` and `getParserK`
- functions. Thus eqs. (1) and (2) become:
-
-     getParserK sortedRules fp = fp               ................. (1')
-
-     getParser = tabulate sortedRules getParserK  ................. (2')
-
- The concept is the same, just that the implementation is abit more awkward.
-
- By enabling the calls to `trace` in lines 127-131, one can prove that this approach
- only parses each rule once.
-
-}


main :: IO ()
main = do
  fname <- getDataFileName "d19.txt"
  input <- readFile fname
  let [rules, messages] = map lines $ splitOn "\n\n" input
      extractRuleNumber :: String -> Int
      extractRuleNumber = read . head . splitOn ":"
      sortedRules       = sortOn extractRuleNumber rules
      getParser         = tabulate sortedRules getParserK  -- eq. (2') above

  putStr "Part 1: "
  let parser0   = getParser 0
      successes = map (parseMessageWith parser0) messages
  print $ length (filter id successes)

  putStr "Part 2: "
  let parser42   = getParser 42
      parser31   = getParser 31
      -- The new rule 0 is parser8' >> parser11', where parser8' is n times
      -- of parser42, and parser11 is m times of parser42 followed by m times
      -- of parser 31. So, altogether, the new rule 0 is (n + m) parser42's
      -- and m parser31's, where n > m > 0.
      newParser0 = do
        n <- some $ try parser42
        m <- some parser31      -- `some` enforces m > 0.
        guard $ length n > length m
        return ()
      successes' = map (parseMessageWith newParser0) messages
  print $ length (filter id successes')


type Parser = Parsec () String

-- The type of `getParser`, i.e. the function to be tabulated.
type ItoP = Int -> Parser ()

-- Tabulation function. The rules must be sorted at this stage so that
-- item number n of `table` corresponds to rule number n. In theory, this
-- is not actually necessary, but it just makes things a lot easier.
-- Furthermore, we only need to do the sorting one time (in `main`).
tabulate :: [String] -> ([String] -> ItoP -> ItoP) -> ItoP
tabulate sortedRules kernel = fp  -- fp is the fixed point, i.e. `getParser`.
 where
  table :: [Parser ()]
  table = map (kernel sortedRules fp) [0 ..]
  fp    = (table !!)


-- Kernel function which recursively depends on f (in line 94).
getParserK :: [String] -> ItoP -> ItoP
getParserK sortedRules fp = fromJust . parseMaybe pRule . (sortedRules !!)
 where
  pRule :: Parser (Parser ())
  pRule = do
    _ <- some digitChar
    _ <- string ": "
    try pOrRule <|> try pAndRule <|> pCharRule
  -- By using the following implementation of pRule instead, we can prove
  -- that pRule is only being called a total of 136 times (there are 136
  -- rules in the input).
  -- pRule = do
  --   ruleNumber <- some digitChar
  --   _          <- string ": "
  --   ruleParser <- try pOrRule <|> try pAndRule <|> pCharRule
  --   return $ ruleNumber `trace` ruleParser

  pOrRule :: Parser (Parser ())
  pOrRule = do
    leftParser  <- pAndRule
    _           <- string "| "
    rightParser <- pAndRule
    return $ void (try leftParser <|> rightParser)
  pAndRule :: Parser (Parser ())
  pAndRule = do
    ruleNumbers <- some $ (read <$> some digitChar) <* space
    return $ mapM_ fp ruleNumbers   -- Sequences the parsers for each rule.
  pCharRule :: Parser (Parser ())
  pCharRule = do
    _ <- char '"'
    c <- anySingle
    _ <- char '"'
    return $ void (char c)


-- Run a parser on a given message.
parseMessageWith :: Parser () -> String -> Bool
parseMessageWith parser message = isJust $ parseMaybe parser message
