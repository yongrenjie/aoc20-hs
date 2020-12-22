import           Data.List                      ( foldl1'
                                                , (\\)
                                                , sortOn
                                                , intercalate
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Paths_aoc20_hs                 ( getDataFileName )

main :: IO ()
main = do
  fname <- getDataFileName "d21.txt"
  input <- readFile fname

  let foods             = map parseFood . lines $ input
      allIngredients    = multiU $ map ingredients foods
      allAllergens      = multiU $ map allergens foods
      allAllergensL     = S.toList allAllergens
      -- Find out which ingredients are possibly bad.
      possibleBadIngrs  = multiU $ map (findPossibleIngr foods) allAllergensL
      -- The remaining ones are definitely good.
      definiteGoodIngrs = allIngredients S.\\ possibleBadIngrs
  putStr "Part 1: "
  print $ sum $ map (S.size . S.intersection definiteGoodIngrs . ingredients) foods

  -- By a process of elimination, we can assign each allergen to one single
  -- ingredient.
  let alleIngrCombos = map (\a -> (a, S.toList $ findPossibleIngr foods a)) allAllergensL
      alleIngrPairs  = solve alleIngrCombos
  -- print $ alleIngrPairs
  putStr "Part 2: "
  putStrLn $ intercalate "," . map (head . snd) . sortOn fst $ alleIngrPairs


type Ingredient = String
type Allergen = String
data Food = Food
  { ingredients :: Set Ingredient
  , allergens   :: Set Allergen
  }
  deriving (Eq, Show)
parseFood :: String -> Food
parseFood t = Food ingrs alles
  where
   [x, y] = splitOn "(contains " t 
   ingrs = S.fromList . words $ x
   alles = S.fromList . splitOn ", " . init $ y


isAllergenIn :: Allergen -> Food -> Bool
allergen `isAllergenIn` food = allergen `S.member` allergens food

-- `findPossibleIngr foods allergen` finds all ingredients in `foods` which can
-- contain the allergen `allergen`.
findPossibleIngr :: [Food] -> Allergen -> Set Ingredient
findPossibleIngr foods allergen = badIngrs
  where
    badFoods     = filter (allergen `isAllergenIn`) foods
    badFoodIngrs = map ingredients badFoods
    badIngrs     = multiI badFoodIngrs

-- Intersection of multiple sets
multiI :: Ord a => [Set a] -> Set a
multiI = foldl1' S.intersection
-- Unioin of multiple sets
multiU :: Ord a => [Set a] -> Set a
multiU = foldl1' S.union

solve :: [(Allergen, [Ingredient])] -> [(Allergen, [Ingredient])]
solve combos = if all ((==1) . length . snd) combos' then combos' else solve combos'
  where
    solvedIngrs = map (head . snd) . filter ((==1) . length . snd) $ combos
    combos' = map (fmap removeSolvedIngrs) combos
    removeSolvedIngrs :: [Ingredient] -> [Ingredient]
    removeSolvedIngrs xs = case length xs of
                                1 -> xs
                                _ -> xs \\ solvedIngrs
