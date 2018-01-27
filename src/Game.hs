{-# LANGUAGE OverloadedStrings #-}
module Game where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import Control.Lens
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict ((!?))
import qualified Data.Map.Strict as Map
import Data.String (fromString)
import Data.Monoid ((<>))
import Linear.V2

import NetworkUtils
import Player
import ServerTypes

frameTime :: Int
frameTime = 1000000 `div` 30

broadcastPlayerPosition ::
  STM.TVar ServerState -> B.ByteString -> B.ByteString -> Client -> IO ()
broadcastPlayerPosition serverStateVar lobbyName playerName client =
  let V2 x y = client ^. clientPlayerData . playerPosition
  in sendToEveryone serverStateVar lobbyName $
       "position:" <> playerName <> " " <>
         fromString (show x) <> " " <> fromString (show y)

updatePlayers :: LobbyState -> LobbyState
updatePlayers =
  lobbyClients.traverse.clientPlayerData %~ updatePlayer

runMainLoop :: STM.TVar ServerState -> B.ByteString -> IO ()
runMainLoop serverStateVar lobbyName = do
  mLobby <- STM.atomically $ do
    let runUpdates lobby = STM.modifyTVar' serverStateVar (at lobbyName . _Just .~ updatePlayers lobby)
    mLobby <- (!? lobbyName) <$> STM.readTVar serverStateVar
    maybe (return ()) runUpdates mLobby
    return mLobby

  case mLobby of
    Just lobby -> do
      let clients = _lobbyClients lobby
      --STM.atomically $ STM.modifyTVar' serverStateVar updatePlayers
      _ <- Map.traverseWithKey (broadcastPlayerPosition serverStateVar lobbyName) clients
      threadDelay frameTime
      runMainLoop serverStateVar lobbyName
    Nothing ->
      BC.putStrLn $ "All users have exited " <> lobbyName <> ". Closing game loop."
