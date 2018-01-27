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

runMainLoop :: STM.TVar ServerState -> B.ByteString -> IO ()
runMainLoop serverStateVar lobbyName = do
  mLobby <- (!? lobbyName) <$> STM.readTVarIO serverStateVar
  case mLobby of
    Nothing -> do
      BC.putStrLn $ "All users have exited " <> lobbyName <> ". Closing game loop."
    Just lobby -> do
      let clients = _lobbyClients lobby
      _ <- Map.traverseWithKey (broadcastPlayerPosition serverStateVar lobbyName) clients
      threadDelay frameTime
      runMainLoop serverStateVar lobbyName
