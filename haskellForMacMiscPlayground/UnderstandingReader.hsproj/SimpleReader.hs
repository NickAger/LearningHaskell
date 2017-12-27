module SimpleReader where
  
-- http://blog.ssanj.net/posts/2014-09-23-A-Simple-Reader-Monad-Example.html
-- A Simple Reader Monad Example

import Control.Monad.Reader

-- Reader r a
-- where r is some “environment” and a is some value you create from that environment. 

tom :: Reader String String
tom = do
    env <- ask -- gives you the environment which in this case is a String
    return (env ++ " This is Tom.")

jerry :: Reader String String
jerry = do
  env <- ask
  return (env ++ " This is Jerry.")

tomAndJerry :: Reader String String
tomAndJerry = do
    t <- tom
    j <- jerry
    return (t ++ "\n" ++ j)

runJerryRun :: String
runJerryRun = (runReader tomAndJerry) "Who is this?"


tomAndJerry2 :: Reader String Int
tomAndJerry2 = do
    t <- tom
    j <- jerry
    return $ length (t ++ "\n" ++ j)
    
runJerryRun2 :: Int
runJerryRun2 = (runReader tomAndJerry2) "Who is this?"