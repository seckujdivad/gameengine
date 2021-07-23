module GameEngineServer.AtomicTChan (sendToTChan, readFromTChan) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan, writeTChan, readTChan)

-- |Transactionally write some data to a 'TChan'
sendToTChan :: TChan a -> a -> IO ()
sendToTChan channel = atomically . writeTChan channel

-- |Transactionally read some data from a 'TChan'
readFromTChan :: TChan a -> IO a
readFromTChan = atomically . readTChan