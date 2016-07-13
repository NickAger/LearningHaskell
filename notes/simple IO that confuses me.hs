import System.Directory

-- hmm perhaps the exmple:
-- getLine >>= readFile >>= putStrLn"
-- from http://adit.io/posts/2013-04-17-functors,_applicatives,_and_monads_in_pictures.htmlx

main = do
--    both these work
--    putStrLn =<< getCurrentDirectory
--    getCurrentDirectory >>= putStrLn

    d <- getCurrentDirectory
    putStrLn d

--   doesn't work
--    putStrLn getCurrentDirectory
    putStrLn "Hello world"