import Data.List  (foldl')
import Data.Maybe (fromJust)
import Text.Megaparsec
import Text.Megaparsec.Char
import Paths_aoc20_hs

main :: IO ()
main = do
    fname <- getDataFileName "d12.txt"
    instructions <- parseInput <$> readFile fname
    putStr "Part 1: "
    print (manhattan $ foldl' moveBoat1 initialBoat1 instructions)
    putStr "Part 2: "
    print (manhattan $ foldl' moveBoat2 initialBoat2 instructions)

type Parser = Parsec String String
data Instruction = North Int | South Int | East Int | West Int
                 | LeftT Int | RightT Int  -- Left and Right collide with Either
                 | Forward Int
                 deriving (Eq, Show)

parseInput :: String -> [Instruction]
parseInput = map (\l -> fromJust $ parseMaybe p l) . lines
    where p :: Parser Instruction
          p = do
              action <- choice [ North   <$ char 'N'
                               , South   <$ char 'S'
                               , East    <$ char 'E'
                               , West    <$ char 'W'
                               , LeftT   <$ char 'L'
                               , RightT  <$ char 'R'
                               , Forward <$ char 'F']
              number <- some digitChar
              return $ action (read number)

data Boat = Boat { x  :: Int    -- current x-position
                 , y  :: Int    -- current y-position
                 , dx :: Int    -- x-extent of current direction
                 , dy :: Int }  -- y-extent of current direction
               deriving (Eq, Show)

changeBearing :: (Int, Int) -> Instruction -> (Int, Int)
changeBearing (p, q) ins
    | ins == LeftT 90  || ins == RightT 270 = (-q,  p)
    | ins == LeftT 180 || ins == RightT 180 = (-p, -q)
    | ins == LeftT 270 || ins == RightT 90  = ( q, -p)
    | otherwise                             = error $ "bad direction: " ++ show ins

initialBoat1 :: Boat
initialBoat1 = Boat 0 0 1 0

moveBoat1 :: Boat -> Instruction -> Boat
moveBoat1 (Boat p q dp dq) ins
    = case ins of         --  x-position   y-position  x-direction y-direction
           North   n -> Boat  p           (q + n)      dp          dq
           South   n -> Boat  p           (q - n)      dp          dq
           East    n -> Boat (p + n)       q           dp          dq
           West    n -> Boat (p - n)       q           dp          dq
           LeftT   _ -> Boat  p            q           dp'         dq'
           RightT  _ -> Boat  p            q           dp'         dq'
           Forward n -> Boat (p + n * dp) (q + n * dq) dp          dq
    where (dp', dq') = changeBearing (dp, dq) ins

initialBoat2 :: Boat
initialBoat2 = Boat 0 0 10 1

moveBoat2 :: Boat -> Instruction -> Boat
moveBoat2 (Boat p q dp dq) ins
    = case ins of         --  x-position   y-position   x-direction y-direction
           North   n -> Boat  p            q            dp         (dq + n)
           South   n -> Boat  p            q            dp         (dq - n)
           East    n -> Boat  p            q           (dp + n)     dq
           West    n -> Boat  p            q           (dp - n)     dq
           LeftT   _ -> Boat  p            q            dp'         dq'
           RightT  _ -> Boat  p            q            dp'         dq'
           Forward n -> Boat (p + n * dp) (q + n * dq)  dp          dq
    where (dp', dq') = changeBearing (dp, dq) ins

manhattan :: Boat -> Int
manhattan = (+) <$> (abs . x) <*> (abs . y)
-- I'm mildly pleased that this worked, even though it's not pretty.
-- Written in a more legible way:
--    manhattan boat = (abs $ x boat) + (abs $ y boat)
