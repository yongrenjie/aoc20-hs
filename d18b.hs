import           Paths_aoc20_hs
import           Data.Char                      ( isSpace )
import           Data.Maybe                     ( fromMaybe )
import           Text.Megaparsec
import           Text.Megaparsec.Char

{-
- For comments on overall strategy, see d18a.hs. I just made a new file for
- this one as I don't want to have to postfix every function with apostrophes.
-}

main :: IO ()
main = do
  fname <- getDataFileName "d18.txt"
  input <- readFile fname
  let exprs = map myParse $ lines input
  print $ sum (map evalExpr1 exprs)

-- Part 2. Different grammar, different parsers.
-- A term is still either a value, or an expression in a bracket.
data Term = Val Int | Bracket Expr1 deriving (Eq, Show)
-- There is a new intermediate-level datatype which is either a term itself, or
-- a term added to an expr2.
data Expr2 = Single2 Term | Addn Term Expr2 deriving (Eq, Show)
-- The highest-level term is either an expr2, or an expr2 multiplied by an expr1.
data Expr1 = Single Expr2 | Mult Expr2 Expr1 deriving (Eq, Show)

-- Evaluate an expression.
evalTerm :: Term -> Int
evalTerm (Val     x ) = x
evalTerm (Bracket e1) = evalExpr1 e1
evalExpr2 :: Expr2 -> Int
evalExpr2 (Single2 t) = evalTerm t
evalExpr2 (Addn t e2) = evalTerm t + evalExpr2 e2
evalExpr1 :: Expr1 -> Int
evalExpr1 (Single e2 ) = evalExpr2 e2
evalExpr1 (Mult e2 e1) = evalExpr2 e2 * evalExpr1 e1

-- Parse a (left-associative) expression.
myParse :: String -> Expr1
myParse =
  fromMaybe (error "bad input")
    . parseMaybe parseExpr1
    . map changeBrackets
    . reverse
    . filter (not . isSpace)
 where
  changeBrackets '(' = ')'
  changeBrackets ')' = '('
  changeBrackets c   = c
type Parser = Parsec String String
parseExpr1 :: Parser Expr1
parseExpr1 = try parseMult <|> (Single <$> parseExpr2)
parseMult :: Parser Expr1
parseMult = do
  expr2 <- parseExpr2
  _     <- char '*'
  Mult expr2 <$> parseExpr1
parseExpr2 :: Parser Expr2
parseExpr2 = try parseAdd <|> (Single2 <$> parseTerm)
parseAdd :: Parser Expr2
parseAdd = do
  t  <- parseTerm
  _  <- char '+'
  Addn t <$> parseExpr2
parseTerm :: Parser Term
parseTerm = try parseVal <|> parseBracket
parseVal :: Parser Term
parseVal = Val . read <$> some digitChar
parseBracket :: Parser Term
parseBracket = Bracket <$> (char '(' *> parseExpr1 <* char ')')
