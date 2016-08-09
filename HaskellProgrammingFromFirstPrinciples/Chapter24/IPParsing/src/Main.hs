module Main where

  {-
  Understanding IP addresses

  172.16.254.1 -> 2886794753

  172 = 0xAC
  16 = 0x10
  254 = 0xFE
  1 = 0x01

  0xAC10FE01 = 2886794753

  1 + (254 * 256) + (16 * 65536) + (172 * 16777216)
  1 + 65024 +  1048576 + 2885681152
  = 2886794753

  -}

main :: IO ()
main = do
  putStrLn "hello world"
