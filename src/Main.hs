{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Main where

import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Lens
import Control.Monad (forever, forM_, unless)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict ((!), (!?), Map)
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

data LobbyState = LobbyState
  { _lobbyClients :: Map B.ByteString Client
  }

data Client = Client
  { _clientConnection :: WS.Connection
  }

makeLenses ''LobbyState
makeLenses ''Client

type ServerState = Map B.ByteString LobbyState


app :: STM.TVar ServerState -> Snap ()
app serverState = Snap.route
  [ ("chat/:lobby/:username", chat serverState)
  , ("", Snap.ifTop $ Snap.serveFile "public/console.html")
  , ("", Snap.serveDirectory "public")
  ]


chat :: STM.TVar ServerState -> Snap ()
chat serverState = do
  Just lobby <- Snap.getParam "lobby"
  Just username <- Snap.getParam "username"
  WS.runWebSocketsSnap $ chatApp serverState lobby username


splitMessage :: B.ByteString -> (B.ByteString, B.ByteString)
splitMessage msg = BC.drop 1 <$> BC.span (/= ':') msg


chatApp :: STM.TVar ServerState -> B.ByteString -> B.ByteString -> WS.ServerApp
chatApp serverStateVar lobbyName username pending = do
  -- Create a new lobby if it doesn't exist
  alreadyExisted <- STM.atomically $ do
    serverState <- STM.readTVar serverStateVar
    let alreadyExisted = Map.member lobbyName serverState
    unless alreadyExisted $ do
      STM.modifyTVar' serverStateVar $
        Map.insert lobbyName $ LobbyState Map.empty
    return alreadyExisted

  unless alreadyExisted $ BC.putStrLn $ "Creating new lobby " <> lobbyName

  BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> " has connected"
  conn <- WS.acceptRequest pending

  let addClientToLobby =
        Map.adjust (lobbyClients %~ Map.insert username (Client conn)) lobbyName

      -- Remove the whole lobby if all clients leave
      lobbyWithClientRemoved lobby =
        let
          updatedLobby = lobbyClients %~ Map.delete username $ lobby
        in
          if Map.null (_lobbyClients updatedLobby) then
            Nothing
          else
            Just updatedLobby

      deleteClientFromLobby =
        Map.update lobbyWithClientRemoved lobbyName
  
  STM.atomically $ STM.modifyTVar' serverStateVar addClientToLobby

  let sendToEveryoneElse str = do
        lobby <- (! lobbyName) <$> STM.readTVarIO serverStateVar
        forM_ (Map.delete username $ _lobbyClients lobby) $ \client ->
          WS.sendTextData (_clientConnection client) str

      removeClient = do
        BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> " has disconnected"
        sendToEveryoneElse $ "disconnected: " <> username
        STM.atomically $ STM.modifyTVar' serverStateVar deleteClientFromLobby

  sendToEveryoneElse $ username <> " has connected"

  let messageDispatcher = Map.fromList
        [ ( "chat"
          , \messageContent -> do
              BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> ": " <> messageContent
              sendToEveryoneElse $ "chat:" <> username <> ": " <> messageContent
          )
        , ( "start game"
          , \_ -> do
              BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> " clicked on start game!"
          )
        ]

  -- Any time a user sends a message,
  -- broadcast it to the other people in the lobby
  flip finally removeClient $ forever $ do
    message <- WS.receiveData conn
    let (messageType, messageContent) = splitMessage message

    case messageDispatcher !? messageType of
      Just dispatchFunction -> dispatchFunction messageContent
      Nothing -> BC.putStrLn $ "Received message of unknown type: " <> message


main :: IO ()
main = do
  serverState <- STM.newTVarIO Map.empty
  Snap.httpServe Snap.defaultConfig (app serverState)
