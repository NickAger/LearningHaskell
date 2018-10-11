module Main where

import Lib

import qualified Servant as S
import Servant ( (:>), (:<|>)(..) )
import qualified Network.Wai.Handler.Warp as W

main :: IO ()
main = do
    putStrLn "booking system startup"
    W.run 8080 app

app = S.serve api server

api :: S.Proxy (PingAPI :<|> PongAPI)
api = S.Proxy

server = handlePing :<|> handlePong

type PingAPI = "ping" :> S.Get '[S.PlainText] String
type PongAPI = "pong" :> S.Get '[S.PlainText] String


handlePing :: S.Handler String
handlePing = return "Hello World !!!"

handlePong :: S.Handler String
handlePong = return "this is pong"