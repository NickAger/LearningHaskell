module Main where

import           Lib

import qualified Servant                       as S
import           Servant                                  ( (:>)
                                                          , (:<|>)(..)
                                                          )
import qualified Network.Wai.Handler.Warp      as W
import           Control.Concurrent.STM        as STM
import           Control.Monad.IO.Class                   ( liftIO )

main :: IO ()
main = do
    putStrLn "booking system startup"
    db <- mkEmptyBookingDB
    addFakeBookings db
    W.run 8080 (app db)

app db = S.serve api (server db)

api
    :: S.Proxy
           (PingAPI :<|> PongAPI :<|> GetBookingTextAPI :<|> GetBookingAPI :<|> GetSingleBookingAPI :<|> PostBookingAPI)
api = S.Proxy

server db =
    handlePing
        :<|> handlePong
        :<|> (handleBookingsText db)
        :<|> (handleBookings db)
        :<|> (handleGetSingleBooking db)
        :<|> (handlePost db)

type PingAPI = "ping" :> S.Get '[S.PlainText] String
type PongAPI = "pong" :> S.Get '[S.PlainText] String
type GetBookingTextAPI = "bookings.txt" :> S.Get '[S.PlainText] String
type GetBookingAPI = "bookings" :> S.Get '[S.JSON] [Booking]
type GetSingleBookingAPI = "bookings" :> S.Capture "id" Int :> S.Get '[S.JSON] Booking
type PostBookingAPI = "bookings" :> S.ReqBody '[S.JSON] Booking :> S.Post '[S.PlainText] String

handlePost :: STM.TVar [Booking] -> Booking -> S.Handler String
handlePost db booking = do
    res <- liftIO $ addBooking db booking
    case res of
        Left  err       -> S.throwError $ S.err500 { S.errReasonPhrase = err }
        Right bookingID -> return (show bookingID)

handleBookingsText db = liftIO $ do
    v <- STM.atomically $ STM.readTVar db
    return (show v)

handleBookings db = liftIO $ STM.atomically $ STM.readTVar db

handleGetSingleBooking :: STM.TVar [Booking] -> Int -> S.Handler Booking
handleGetSingleBooking db booking_id = do
    v <- liftIO $ STM.atomically $ STM.readTVar db
    let booking = filter (\b -> getID b == booking_id) v
    case booking of
        []  -> S.throwError S.err404
        [v] -> return v
        _   -> S.throwError S.err500

handlePing :: S.Handler String
handlePing = return "Hello World !!!"

handlePong :: S.Handler String
handlePong = return "this is pong"

addFakeBookings db = do
    let (Right r1) = mkBooking 9 12 "first"
    let (Right r2) = mkBooking 21 24 "second"
    addBooking db r1
    addBooking db r2
