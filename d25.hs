-- Hardcode it for today :-)
cardKey, doorKey, succBoxing :: Integer
cardKey = 10212254
doorKey = 12577395
succBoxing = 20201227

main :: IO ()
main = do
  putStr "Part 1: "
  let doorLoop = getLoopSize doorKey
  print $ getEncryption cardKey doorLoop

getLoopSize :: Integer -> Integer
getLoopSize key = go 0 1
  where
    go m n = if n `rem` succBoxing == key
                then m
                else go (m + 1) (n * 7 `rem` succBoxing)

getEncryption :: Integer -> Integer -> Integer
getEncryption pubKey loops = (pubKey ^ loops) `rem` succBoxing
