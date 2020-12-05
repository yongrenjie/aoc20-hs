{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO
import Data.Maybe
import Data.Monoid
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d4.txt"
    input <- TIO.readFile fname
    putStr $ "Part 1: "
    print $ length . filter isJust . findValidPassports False $ input
    putStr $ "Part 2: "
    print $ length . filter isJust . findValidPassports True $ input

data PassportField = BirthYear T.Text
                   | IssueYear T.Text
                   | ExpirationYear T.Text
                   | Height T.Text
                   | HairColor T.Text
                   | EyeColor T.Text
                   | PassportID T.Text
                   | CountryID T.Text
                   deriving (Eq, Show)

data Passport = Passport { byr :: PassportField
                         , iyr :: PassportField
                         , eyr :: PassportField
                         , hgt :: PassportField
                         , hcl :: PassportField
                         , ecl :: PassportField
                         , pid :: PassportField
                         , cid :: PassportField
                         }
              deriving (Show)

-- splits input into passport-sized lists. each list entry is a field text.
getFieldTexts :: T.Text -> [[T.Text]]
getFieldTexts = map T.words . T.splitOn "\n\n"

-- finds a PassportField inside a list of field texts
findField :: Bool -> T.Text -> [T.Text] -> Maybe PassportField
findField isPartTwo desiredFieldName = getFirst . foldMap (First . checkF desiredFieldName)
    where checkF = if isPartTwo then checkField' else checkField

-- scans one field text to see whether it matches the desired field name
-- returns the PassportField if it is
checkField :: T.Text -> T.Text -> Maybe PassportField
checkField desiredFieldName field =
    if fieldName /= desiredFieldName
       then Nothing
       else case fieldName of
                 "byr" -> Just (BirthYear fieldValue)
                 "iyr" -> Just (IssueYear fieldValue)
                 "eyr" -> Just (ExpirationYear fieldValue)
                 "hgt" -> Just (Height fieldValue)
                 "hcl" -> Just (HairColor fieldValue)
                 "ecl" -> Just (EyeColor fieldValue)
                 "pid" -> Just (PassportID fieldValue)
                 "cid" -> Just (CountryID fieldValue)
                 _     -> error "invalid field requested"
    where (fieldName:fieldValue:_) = T.split (==':') field

-- checkField but with additional validation for part 2
checkField' :: T.Text -> T.Text -> Maybe PassportField
checkField' desiredFieldName field =
    if fieldName /= desiredFieldName
       then Nothing
       else case fieldName of
                 "byr" -> BirthYear <$> validateBounds 1920 2002 fieldValue
                 "iyr" -> IssueYear <$> validateBounds 2010 2020 fieldValue
                 "eyr" -> ExpirationYear <$> validateBounds 2020 2030 fieldValue
                 "hgt" -> Height <$> validateHeight fieldValue
                 "hcl" -> HairColor <$> validateHCL fieldValue
                 "ecl" -> EyeColor <$> validateECL fieldValue
                 "pid" -> PassportID <$> validatePID fieldValue
                 "cid" -> Just (CountryID fieldValue)
                 _     -> error "invalid field requested"
    where (fieldName:fieldValue:_) = T.split (==':') field

-- checks whether a string is between low and high (inclusive)
-- returns Nothing if it's not an integer of if it's out of bounds
-- returns Just the string if it is valid
validateBounds :: Integer -> Integer -> T.Text -> Maybe T.Text
validateBounds low high text =
    case TR.decimal text of
         Right (a, _) -> if low <= a && a <= high then Just text else Nothing
         Left _ -> Nothing

validateHeight :: T.Text -> Maybe T.Text
validateHeight text =
    case TR.decimal text of
         Right (a, "cm") -> if 150 <= a && a <= 193 then Just text else Nothing
         Right (a, "in") -> if 59 <= a && a <= 76 then Just text else Nothing
         _ -> Nothing

validateHCL :: T.Text -> Maybe T.Text
validateHCL text =
    case (T.head text, T.length text) of
         ('#', 7) -> if T.all (`elem` ("ABCDEFabcdef1234567890" :: String)) (T.tail text)
                        then Just text else Nothing
         _ -> Nothing

validateECL :: T.Text -> Maybe T.Text
validateECL text = if elem text ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]
                      then Just text
                      else Nothing

validatePID :: T.Text -> Maybe T.Text
validatePID text =
    case T.length text of
         9 -> if T.all (`elem` ("1234567890" :: String)) text then Just text else Nothing
         _ -> Nothing


-- parses a line of text into a Passport.
makePassport :: Bool -> [T.Text] -> Maybe Passport
makePassport isPartTwo fieldTexts = Passport <$> findField isPartTwo "byr" fieldTexts
                                             <*> findField isPartTwo "iyr" fieldTexts
                                             <*> findField isPartTwo "eyr" fieldTexts
                                             <*> findField isPartTwo "hgt" fieldTexts
                                             <*> findField isPartTwo "hcl" fieldTexts
                                             <*> findField isPartTwo "ecl" fieldTexts
                                             <*> findField isPartTwo "pid" fieldTexts
                                             <*> Just (CountryID "doesntmatter")

-- parses the input using all the previous functions.
findValidPassports :: Bool -> T.Text -> [Maybe Passport]
findValidPassports isPartTwo input = map (makePassport isPartTwo) . getFieldTexts $ input
