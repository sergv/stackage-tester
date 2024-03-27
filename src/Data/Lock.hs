-- |
-- Module:     Data.Lock
-- Copyright:  (c) Sergey Vinokurov 2024
-- License:    Apache-2.0 (see LICENSE)
-- Maintainer: serg.foo@gmail.com

module Data.Lock
  ( Lock
  , Resource
  , new
  , withAcquired
  ) where

import Control.Concurrent.MVar
import Control.Exception

newtype Lock a = Lock { unLock :: MVar (Resource a) }

data Resource a = Resource

new :: IO (Lock a)
new = Lock <$> newMVar Resource

acquire :: Lock a -> IO (Resource a)
acquire = takeMVar . unLock

release :: Resource a -> Lock a -> IO ()
release !res (Lock v) = putMVar v res

withAcquired :: Lock b -> IO a -> IO a
withAcquired lock action =
  bracket (acquire lock) (`release` lock) $ \_ -> action
