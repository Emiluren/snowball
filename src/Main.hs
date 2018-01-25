{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Monad (forever)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

type ServerState = Map B.ByteString WS.Connection


app :: STM.TVar ServerState -> Snap ()
app serverState = Snap.route
    [ ("", Snap.ifTop $ Snap.serveFile "console.html")
    , ("console.js", Snap.serveFile "console.js")
    , ("chat/:username", chat serverState)
    , ("style.css", Snap.serveFile "style.css")
    ]


chat :: STM.TVar ServerState -> Snap ()
chat serverState = do
    Just username <- Snap.getParam "username"
    WS.runWebSocketsSnap $ chatApp serverState username


chatApp :: STM.TVar ServerState -> B.ByteString -> WS.ServerApp
chatApp serverState username pending = do
    BC.putStrLn $ username <> " has connected"
    conn <- WS.acceptRequest pending
    STM.atomically $ STM.modifyTVar' serverState (Map.insert username conn)
    
    let removeClient = do
          BC.putStrLn $ username <> " has disconnected"
          STM.atomically $ STM.modifyTVar' serverState (Map.delete username)

    flip finally removeClient $ forever $ do
      message <- WS.receiveData conn
      BC.putStrLn $ username <> ": " <> message


main :: IO ()
main = do
  serverState <- STM.newTVarIO Map.empty
  Snap.httpServe Snap.defaultConfig (app serverState)
