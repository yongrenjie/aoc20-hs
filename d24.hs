import           Data.Bifunctor
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromMaybe )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Paths_aoc20_hs                 ( getDataFileName )

main :: IO ()
main = do
  fname <- getDataFileName "d24.txt"
  input <- readFile fname

  putStr "Part 1: "
  let tiles = map (fromMaybe (error "bad parse") . parseMaybe pLine) $ lines input
      blackTiles = getBlackTiles tiles
  print $ S.size blackTiles

  putStr "Part 2: "
  print $ S.size . (!! 100) . iterate update $ blackTiles


type Parser = Parsec () String
type Tile = (Int, Int)

pLine :: Parser Tile
pLine = do
  directions <- many $ choice
    [ (-1, 0) <$ char 'w'
    , (1, 0) <$ char 'e'
    , (1, -1) <$ try (string "se")
    , (0, -1) <$ string "sw"
    , (0, 1) <$ try (string "ne")
    , (-1, 1) <$ string "nw"
    ]
  pure $ bimap sum sum . unzip $ directions

getBlackTiles :: [Tile] -> Set Tile
getBlackTiles = foldl' f S.empty
  where
    f :: Set Tile -> Tile -> Set Tile
    f ts t = if t `S.member` ts then S.delete t ts else S.insert t ts

update :: Set Tile -> Set Tile
update ts = ts'
  where
    blackTilesToCheck = ts
    whiteTilesToCheck = S.unions $ S.map getNeighbours ts
    blackTilesToFlip = S.filter (((||) <$> (==0) <*> (>2)) . adjacentBlack) blackTilesToCheck
    whiteTilesToFlip = S.filter ((==2) . adjacentBlack) whiteTilesToCheck
    getNeighbours :: Tile -> Set Tile
    getNeighbours (x, y) = S.fromList [(x-1, y), (x+1, y), (x+1, y-1), (x, y-1), (x, y+1), (x-1, y+1)]
    adjacentBlack :: Tile -> Int
    adjacentBlack = S.size . S.filter (`elem` ts) . getNeighbours
    ts' = S.union ts whiteTilesToFlip S.\\ blackTilesToFlip
