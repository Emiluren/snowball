module Player where

import Linear
import Data.Queue.Instances


data PlayerEvents = Jump | Throw (V2 Float)

data Player = Player
  { leftPressed :: Bool
  , rightPressed :: Bool
  , health :: Int
  , position :: V2 Float
  , events :: Chan PlayerEvents
  }



