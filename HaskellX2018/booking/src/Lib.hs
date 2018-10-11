module Lib
    ( someFunc
    , Booking
    , mkBooking
    , overlaps
    , consBooking
    , mkEmptyBookingDB
    , addBooking
    ) where

import qualified Data.Text as T
import Control.Monad (when)
import Control.Concurrent.STM as STM

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

overlaps :: Booking -> Booking -> Bool
overlaps (Booking ls le ld) (Booking rs re rd) = ls < re && rs < le

consBooking :: Booking -> [Booking] -> Either String [Booking]
consBooking b bs =
    if or (map (overlaps b) bs)
    then Left "new booking overlaps existing booking"
    else return (b : bs)

mkEmptyBookingDB :: IO (STM.TVar [Booking])
mkEmptyBookingDB = STM.atomically $ STM.newTVar [] :: IO (STM.TVar [Booking])

addBooking :: STM.TVar [Booking] -> Booking -> IO (Either String Int)
addBooking db booking = STM.atomically $ do
    oldBookings <- STM.readTVar db
    let newBookings = booking `consBooking` oldBookings
    case newBookings of
        Left err -> return (Left err)
        Right b' -> do
            STM.writeTVar db b'
            pure (Right (_start booking))

