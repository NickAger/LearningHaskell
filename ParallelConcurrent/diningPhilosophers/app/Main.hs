module Main where

import           Control.Monad
import           Control.Concurrent
import           Control.Concurrent.STM
import           System.Random
import           Text.Printf
import           Debug.Trace


-- Forks
type Fork = TMVar Int

newFork :: Int -> IO Fork
newFork i = newTMVarIO i

takeFork :: Fork -> STM Int
takeFork fork = takeTMVar fork

releaseFork :: Int -> Fork -> STM ()
releaseFork i fork = putTMVar fork i

-- Philosophers
runPhilosopher :: String -> (Fork, Fork) -> IO ()
runPhilosopher name (left, right) = forever $ do
    putStrLn (name ++ " is hungry.")

    (leftNum, rightNum) <- atomically $ do
        leftNum  <- trace (name ++ " trying to grab left fork") takeFork left
        rightNum <- trace (name ++ " trying to grab right fork") takeFork right
        return (leftNum, rightNum)

    putStrLn $ printf "%s got forks %d and %d and is now eating" name leftNum rightNum
    delay <- randomRIO (1, 3)
    threadDelay (delay * 1000000)
    putStrLn (name ++ " is done eating. Going back to thinking.")

    atomically $ do
        releaseFork leftNum  left
        releaseFork rightNum right

    delay <- randomRIO (1, 3)
    threadDelay (delay * 1000000)

philosophers :: [String]
philosophers = ["Aristotle", "Kant", "Spinoza", "Marx", "Russel"]

main = do
    forks <- mapM newFork [1 .. 5]
    let namedPhilosophers     = map runPhilosopher philosophers
        forkPairs             = zip forks (tail . cycle $ forks)
        philosophersWithForks = zipWith ($) namedPhilosophers forkPairs

    putStrLn "Running the philosophers. Press enter to quit."

    mapM_ forkIO philosophersWithForks

    -- All threads exit when the main thread exits.
    getLine

