{-# LANGUAGE OverloadedStrings #-}

import Data.Char
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO
import Paths_aoc20_hs
import Text.Megaparsec
import Text.Megaparsec.Char

main :: IO ()
main = do
    fname <- getDataFileName "d4.txt"
    input <- TIO.readFile fname
    putStr "Part 1: "
    print $ length . filter isJust . getPassports False $ input
    putStr "Part 2: "
    print $ length . filter isJust . getPassports True $ input
    putStr "Part 1 using megaparsec: "
    print $ length . filter isJust . getPassports' False $ input
    putStr "Part 2 using megaparsec: "
    print $ length . filter isJust . getPassports' True $ input

data Passport = Passport { byr :: T.Text , iyr :: T.Text , eyr :: T.Text
                         , hgt :: T.Text , hcl :: T.Text , ecl :: T.Text
                         , pid :: T.Text , cid :: T.Text }
              deriving (Show)

-- Converts the input string into a list of valid passports (Just Passport) or
-- invalid ones (Nothing).
getPassports :: Bool -> T.Text -> [Maybe Passport]
getPassports isPartTwo = map (makePassport . ppTextToMap) . getPPTexts
    where makePassport = if isPartTwo then makePassportTough else makePassportLax


{- For Part 1 of the question -}

-- Splits input into passport-sized strings
getPPTexts :: T.Text -> [T.Text]
getPPTexts = T.splitOn "\n\n"

-- Parses passport-sized strings into a Map
ppTextToMap :: T.Text -> M.Map T.Text T.Text
ppTextToMap = M.fromList . map parseWords . T.words
    where parseWords :: T.Text -> (T.Text, T.Text)
          parseWords word = (fieldName, fieldValue)
              where (fieldName:fieldValue:_) = T.split (==':') word

-- Parses a Map into a Passport, but with no validation except whether the
-- field is present.
makePassportLax :: M.Map T.Text T.Text -> Maybe Passport
makePassportLax m = Passport <$> M.lookup "byr" m
                             <*> M.lookup "iyr" m
                             <*> M.lookup "eyr" m
                             <*> M.lookup "hgt" m
                             <*> M.lookup "hcl" m
                             <*> M.lookup "ecl" m
                             <*> M.lookup "pid" m
                             <*> Just "AnyCountry" --  M.lookup "cid" m


{- For Part 2 of the question -}

-- Parses a Map into a Passport, but depends on whether the field is present
-- (M.lookup) as well as whether it satisfies certain properties
makePassportTough :: M.Map T.Text T.Text -> Maybe Passport
makePassportTough m = Passport <$> (M.lookup "byr" m >>= validateBounds 1920 2002)
                               <*> (M.lookup "iyr" m >>= validateBounds 2010 2020)
                               <*> (M.lookup "eyr" m >>= validateBounds 2020 2030)
                               <*> (M.lookup "hgt" m >>= validateHeight)
                               <*> (M.lookup "hcl" m >>= validateHCL)
                               <*> (M.lookup "ecl" m >>= validateECL)
                               <*> (M.lookup "pid" m >>= validatePID)
                               <*> Just "AnyCountry" --  M.lookup "cid" m

-- Checks whether a string is an integer between low and high (inclusive).
-- Returns Nothing if it's not an integer or if it's out of bounds, or Just the
-- string if it is valid. We can use this function for the byr, iyr, and eyr
-- fields.
validateBounds :: Integer -> Integer -> T.Text -> Maybe T.Text
validateBounds low high text =
    case TR.decimal text of
         Right (a, _) -> if low <= a && a <= high then Just text else Nothing
         Left _ -> Nothing

-- The remaining validation functions are simply the same as that quoted in the
-- problem specification.

validateHeight :: T.Text -> Maybe T.Text
validateHeight text =
    case TR.decimal text of
         -- the explicit type just silences a -Wtype-defaults ghc warning. not
         -- actually critical
         Right (a, "cm") -> if (150 :: Integer) <= a && a <= 193
                               then Just text else Nothing
         Right (a, "in") -> if 59 <= a && a <= 76
                               then Just text else Nothing
         _ -> Nothing

validateHCL :: T.Text -> Maybe T.Text
validateHCL text = if headIsHash && tailLengthIs6 && tailIsHex
                      then Just text
                      else Nothing
    where headIsHash    = T.head text == '#'
          tailLengthIs6 = T.length text == 7
          tailIsHex     = T.all (`elem` ("ABCDEFabcdef1234567890" :: String)) (T.tail text)

validateECL :: T.Text -> Maybe T.Text
validateECL text = if text `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                      then Just text
                      else Nothing

validatePID :: T.Text -> Maybe T.Text
validatePID text = if lengthIs9 && isNumeric then Just text else Nothing
    where lengthIs9 = T.length text == 9
          isNumeric = T.all (`elem` ("1234567890" :: String)) text



{- An alternative parser with megaparsec -}
getPassports' :: Bool -> T.Text -> [Maybe Passport]
getPassports' isPartTwo = map (makePassport . ppTextToMap') . getPPTexts
    where makePassport = if isPartTwo then makePassportTough else makePassportLax

type Parser = Parsec T.Text T.Text
ppTextToMap' :: T.Text -> M.Map T.Text T.Text
ppTextToMap' = either (const M.empty) M.fromList . runParser (many p) ""
    where p :: Parser (T.Text, T.Text)
          p = do
              space
              fieldName <- T.pack <$> many letterChar
              -- letterChar :: Parser Char
              -- many :: Parser Char -> Parser [Char]
              -- but [Char] is String, not T.Text, so we need to manually wrap
              _ <- char ':'
              fieldValue <- T.pack <$> some (satisfy (not . isSpace))
              return (fieldName, fieldValue)
