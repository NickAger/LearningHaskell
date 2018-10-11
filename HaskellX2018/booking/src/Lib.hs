module Lib
    ( someFunc
    , Booking
    , mkBooking
    ) where

import qualified Data.Text as T
import Control.Monad (when)



someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Booking = Booking {
    _start :: Int,
    _end :: Int,
    _description :: T.Text
} deriving Show

mkBooking :: Int -> Int -> T.Text -> Either String Booking
mkBooking s e d = do
    when (e < s) $ Left "end is before start"
    when (e > 24) $ Left "end is too late"
    when (s < 0 ) $ Left "start is too early"
    when (d == "") $ Left "you must give a description"
    return (Booking { _start = s, _end = e, _description = d }) 