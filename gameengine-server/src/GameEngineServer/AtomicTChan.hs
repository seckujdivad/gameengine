module GameEngineServer.AtomicTChan (sendToTChan, readFromTChan, readAllFromTChan) where

import Control.Monad.STM (atomically, STM)
import Control.Concurrent.STM.TChan (TChan, writeTChan, readTChan, isEmptyTChan)

-- |Transactionally write some data to a 'TChan'
sendToTChan :: TChan a -> a -> IO ()
sendToTChan channel = atomically . writeTChan channel

-- |Transactionally read some data from a 'TChan'
readFromTChan :: TChan a -> IO a
readFromTChan = atomically . readTChan

-- |Read each item from a 'TChan' until it is empty
readAllFromTChan :: TChan a -> IO [a]
readAllFromTChan channel = atomically $ do
    startedEmpty <- isEmptyTChan channel
    if startedEmpty then
        return []
    else
        readAllFromTChanInner channel

readAllFromTChanInner :: TChan a -> STM [a]
readAllFromTChanInner channel = do
    x <- readTChan channel
    isEmpty <- isEmptyTChan channel
    if isEmpty then
        return [x]
    else do
        xs <- readAllFromTChanInner channel
        return (x:xs)