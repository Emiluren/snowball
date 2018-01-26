module NetworkUtils where

import qualified Control.Concurrent.STM as STM
import Control.Monad (forM_)
import qualified Data.ByteString as B
import Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

import ServerTypes

sendToEveryone :: STM.TVar ServerState -> B.ByteString -> B.ByteString -> IO ()
sendToEveryone serverStateVar lobbyName str = do
  lobby <- (! lobbyName) <$> STM.readTVarIO serverStateVar
  forM_ (_lobbyClients lobby) $ \client ->
    WS.sendTextData (_clientConnection client) str

sendToEveryoneElse ::
  STM.TVar ServerState -> B.ByteString -> B.ByteString -> B.ByteString -> IO ()
sendToEveryoneElse serverStateVar lobbyName username str = do
  lobby <- (! lobbyName) <$> STM.readTVarIO serverStateVar
  forM_ (Map.delete username $ _lobbyClients lobby) $ \client ->
    WS.sendTextData (_clientConnection client) str

