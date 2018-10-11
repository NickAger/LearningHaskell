module Main where

import Lib

import qualified Servant as S
import Servant ( (:>), (:<|>)(..) )
import qualified Network.Wai.Handler.Warp as W
import Control.Concurrent.STM as STM
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    putStrLn "booking system startup"
    db <- mkEmptyBookingDB
    addFakeBookings db
    W.run 8080 (app db)

app db = S.serve api (server db)

api :: S.Proxy (PingAPI :<|> PongAPI :<|> GetBookingTextAPI)
api = S.Proxy

server db = handlePing :<|> handlePong :<|> (handleBookingsText db)

type PingAPI = "ping" :> S.Get '[S.PlainText] String
type PongAPI = "pong" :> S.Get '[S.PlainText] String
type GetBookingTextAPI = "bookings.txt" :> S.Get '[S.PlainText] String

handleBookingsText db = liftIO $ do
    v <- STM.atomically $ STM.readTVar db
    return (show v)

handlePing :: S.Handler String
handlePing = return "Hello World !!!"

handlePong :: S.Handler String
handlePong = return "this is pong"

addFakeBookings db = do
    let (Right r1) = mkBooking 9 12 "first"
    let (Right r2) = mkBooking 21 24 "second"
    addBooking db r1
    addBooking db r2