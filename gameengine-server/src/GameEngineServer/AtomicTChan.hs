module GameEngineServer.AtomicTChan (readAllFromTChan) where

import Control.Monad.STM (STM)
import Control.Concurrent.STM.TChan (TChan, readTChan, isEmptyTChan)


-- |Read each item from a 'TChan' until it is empty
readAllFromTChan :: TChan a -> STM [a]
readAllFromTChan channel = do
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