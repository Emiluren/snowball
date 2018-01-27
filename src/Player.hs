{-# LANGUAGE TemplateHaskell #-}
module Player where

import Control.Concurrent.STM.TChan
import Control.Lens
import Linear

data PlayerEvents = Jump | Throw (V2 Float)

data Player = Player
  { _playerLeftPressed :: Bool
  , _playerRightPressed :: Bool
  , _playerHealth :: Int
  , _playerPosition :: V2 Float
  , _playerEvents :: TChan PlayerEvents
  }

newPlayer :: IO Player
newPlayer = do
  chan <- newTChanIO
  return Player
    { _playerLeftPressed = False
    , _playerRightPressed = False
    , _playerHealth = 100
    , _playerPosition = V2 0 0
    , _playerEvents = chan
    }

makeLenses ''Player
