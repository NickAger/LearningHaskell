module OutsideIn where

hypo :: IO ()
hypo = do
  let x :: Int
      x = undefined
  s <- getLine
  
  case s of
    "hi" -> print x
    _ -> putStrLn "hello"
