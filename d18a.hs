import           Paths_aoc20_hs
import           Data.Char                      ( isSpace )
import           Data.Maybe                     ( fromMaybe )
import           Text.Megaparsec
import           Text.Megaparsec.Char

{-
- This is such a classic Haskell thing.
-
- However, I can't figure out how to get the recursive descent parser
- to work for this left-associative problem. Basically we have the
- general structure of
-
-    expr = term || expr + term || expr * term
-
- which suggests that an expression parser `parseExpr` should call
- itself recursively, like this:
- 
-  parseExpr = do
-     expr1 <- parseExpr
-     _     <- char '+'
-     term2 <- parseTerm
-     return $ Op (+) expr1 term2
-
- The problem is that `parseExpr` will call itself at the start of the
- do-block, with the exact same input that it was initially given, so
- we end up in an infinite loop...
-
- To get around this, I reverse the given input (making sure to swap
- open and close brackets). This changes the grammar to being
- right-associative. Since we only have + and * as operators, which are
- both commutative, we don't need to worry about commutativity. I also
- remove the spaces from the inputs so that I don't have to stick lots
- of calls to `space` in the parsers.
-
- Part 2 is pretty much the same, but I brute forced it. It appears that
- Parsec has tools to deal with associativity and operator precedence,
- but Megaparsec doesn't, so I might have made a mistake for today :-)
- But at least I got some serious practice implementing it myself.
-
-}

main :: IO ()
main = do
  fname <- getDataFileName "d18.txt"
  input <- readFile fname
  let exprs = map myParse $ lines input
  print $ sum (map evalExpr exprs)

-- Part 1
-- The operator is either (+) or (*).
type Op = Int -> Int -> Int
-- A term is either a value, or an expression in a bracket.
data Term = Val Int | Bracket Expr
-- An expression is either a term, or the application of an operator to two terms.
data Expr = Single Term | Cmpd Op Term Expr

-- Evaluate an expression.
evalTerm :: Term -> Int
evalTerm (Val x) = x
evalTerm (Bracket e) = evalExpr e
evalExpr :: Expr -> Int
evalExpr (Single t) = evalTerm t
evalExpr (Cmpd op t e) = evalTerm t `op` evalExpr e

-- Parse a (left-associative) expression.
myParse :: String -> Expr
myParse = fromMaybe (error "bad input")
          . parseMaybe parseExpr
          . map changeBrackets
          . reverse
          . filter (not . isSpace)
  where
    changeBrackets '(' = ')'
    changeBrackets ')' = '('
    changeBrackets c   = c
type Parser = Parsec String String
parseExpr :: Parser Expr
parseExpr = try parseCmpd <|> (Single <$> parseTerm)
parseCmpd :: Parser Expr
parseCmpd = do
  term <- parseTerm
  op   <- choice [(+) <$ char '+', (*) <$ char '*']
  Cmpd op term <$> parseExpr
parseTerm :: Parser Term
parseTerm = try parseVal <|> parseBracket
parseVal :: Parser Term
parseVal = Val . read <$> some digitChar
parseBracket :: Parser Term
parseBracket = Bracket <$> (char '(' *> parseExpr <* char ')')
