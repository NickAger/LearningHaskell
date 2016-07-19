import Control.Monad

-- Write bind in terms of fmap and join.
bind :: Monad m => (a -> m b) -> m a -> m b
bind f x = join $ fmap f x

sequencing :: IO ()
sequencing = do
  putStrLn "blah"
  putStrLn "another thing"
  
sequencing' :: IO ()
sequencing' = 
  putStrLn "blah" >>
  putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' = 
  putStrLn "blah" *>
  putStrLn "another thing"

binding :: IO ()
binding = do
  name <- getLine
  putStrLn name
  
binding' :: IO ()
binding' = do
  getLine >>= putStrLn
  
bindingAndSequencing :: IO ()
bindingAndSequencing = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn ("y helo thar: " ++ name)
  
bindingAndSequencing' :: IO ()
bindingAndSequencing' =  putStrLn "name pls:" >>  getLine >>=  \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
  putStrLn "name pls:"
  name <- getLine
  putStrLn "age pls:"
  age <- getLine
  putStrLn ("y helo thar: "
    ++ name ++ " who is: "
    ++ age ++ " years old.")
    
twoBinds' :: IO ()
twoBinds' =  putStrLn "name pls:" >> 
  getLine >>=  \name ->  putStrLn "age pls:" >> 
  getLine >>=  \age ->  putStrLn ("y helo thar: "
    ++ name ++ " who is: " 
    ++ age ++ " years old.")
    
