{-# LANGUAGE TemplateHaskell #-}
module ServerTypes where

import Control.Lens
import qualified Data.ByteString as B
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Network.WebSockets as WS

import Player

data LobbyState = LobbyState
  { _lobbyClients :: Map B.ByteString Client
  , _lobbyGameStarted :: Bool
  --, _lobbyThreadId :: ThreadId
  }

data Client = Client
  { _clientConnection :: WS.Connection
  , _clientPlayerData :: Player
  }

makeLenses ''LobbyState
makeLenses ''Client

type ServerState = Map B.ByteString LobbyState


