main = print $ maxC 1 500000

collatz :: Integer -> Int
collatz 0      =  0
collatz 1      =  0
collatz n
  | even n     =  1 + collatz (n `div` 2)
  | otherwise  =  1 + collatz (3 * n + 1)

maxC :: Int -> Int -> Int
maxC lo hi = maximum (map (collatz . fromIntegral) [lo..hi])
