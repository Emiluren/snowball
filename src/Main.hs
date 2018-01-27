{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Exception (finally)
import Control.Lens
import Control.Monad (forever, unless, when)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict ((!), (!?))
import qualified Data.Map.Strict as Map
import Data.Monoid ((<>))
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Snap as WS
import Snap.Core (Snap)
import qualified Snap.Core as Snap
import qualified Snap.Http.Server as Snap
import qualified Snap.Util.FileServe as Snap

import qualified Game
import NetworkUtils
import Player
import ServerTypes

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
  createdNewLobby <- STM.atomically $ do
    serverState <- STM.readTVar serverStateVar
    let alreadyExisted = Map.member lobbyName serverState
    unless alreadyExisted $ do
      STM.modifyTVar' serverStateVar $
        Map.insert lobbyName $ LobbyState Map.empty False
    return $ not alreadyExisted

  when createdNewLobby $ BC.putStrLn $ "Creating new lobby " <> lobbyName

  BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> " has connected"
  conn <- WS.acceptRequest pending

  let addClientToLobby player =
        -- TODO: add actual player init
        Map.adjust (lobbyClients %~ Map.insert username (Client conn player)) lobbyName

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

  player <- newPlayer
  STM.atomically $ STM.modifyTVar' serverStateVar $ addClientToLobby player

  let removeClient = do
        BC.putStrLn $ "(" <> lobbyName <> ") " <> username <> " has disconnected"
        sendToEveryoneElse serverStateVar lobbyName username $
          "disconnected: " <> username
        STM.atomically $ STM.modifyTVar' serverStateVar deleteClientFromLobby

  sendToEveryoneElse serverStateVar lobbyName username $
    username <> " has connected"

  let messageDispatcher = Map.fromList
        [ ( "chat"
          , \messageContent -> do
              BC.putStrLn $
                "(" <> lobbyName <> ") " <> username <> ": " <> messageContent
              sendToEveryoneElse serverStateVar lobbyName username $
                "chat:" <> username <> ": " <> messageContent
          )
        , ( "start game"
          , \_ -> do
              BC.putStrLn $
                "(" <> lobbyName <> ") " <> username <> " clicked on start game!"

              -- Start a new server main loop thread if there is not already one
              startNewGame <- STM.atomically $ do
                lobby <- (! lobbyName) <$> STM.readTVar serverStateVar
                if _lobbyGameStarted lobby
                  then return False
                  else do STM.modifyTVar' serverStateVar $
                            at lobbyName . _Just . lobbyGameStarted .~ True
                          return True

              when startNewGame $ do
                lobby <- (! lobbyName) <$> STM.readTVarIO serverStateVar
                let clientNames = B.intercalate " " $ Map.keys $ _lobbyClients lobby

                BC.putStrLn $ "(" <> lobbyName <> ") " <> "Starting game with " <> clientNames
                sendToEveryone serverStateVar lobbyName $ "start game:" <> clientNames
                _ <- forkIO $ Game.runMainLoop serverStateVar lobbyName
                return ()
          )
        , ( "key down"
          , \keyName ->
              if keyName == "left" then
                STM.atomically $ STM.modifyTVar' serverStateVar $
                  (at lobbyName)._Just.lobbyClients.(at username)._Just.clientPlayerData.playerLeftPressed .~ True
              else if keyName == "right" then
                STM.atomically $ STM.modifyTVar' serverStateVar $
                  (at lobbyName)._Just.lobbyClients.(at username)._Just.clientPlayerData.playerRightPressed .~ True
              else
                BC.putStrLn $ "unknown key down: " <> keyName
          )
        , ( "key up"
          , \keyName ->
              if keyName == "left" then
                STM.atomically $ STM.modifyTVar' serverStateVar $
                  (at lobbyName)._Just.lobbyClients.(at username)._Just.clientPlayerData.playerLeftPressed .~ False
              else if keyName == "right" then
                STM.atomically $ STM.modifyTVar' serverStateVar $
                  (at lobbyName)._Just.lobbyClients.(at username)._Just.clientPlayerData.playerRightPressed .~ False
              else
                BC.putStrLn $ "unknown key up: " <> keyName
          )
        ]

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
