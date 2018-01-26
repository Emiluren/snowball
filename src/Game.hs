{-# LANGUAGE OverloadedStrings #-}
module Game where

import Control.Concurrent
import qualified Control.Concurrent.STM as STM
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Data.Map.Strict ((!?))
import Data.Monoid ((<>))

import ServerTypes
import NetworkUtils

runMainLoop :: STM.TVar ServerState -> B.ByteString -> IO ()
runMainLoop serverStateVar lobbyName = do
  mLobby <- (!? lobbyName) <$> STM.readTVarIO serverStateVar
  case mLobby of
    Nothing -> do
      BC.putStrLn $ "All users have exited " <> lobbyName <> ". Closing game loop."
    Just lobby -> do
      sendToEveryone serverStateVar lobbyName $ "chat: this is a lie"
      threadDelay 1000000
      runMainLoop serverStateVar lobbyName
