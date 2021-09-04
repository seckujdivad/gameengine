module GameEngineServer.Mainloop.Mainloop (serverMainloop) where

import Control.Monad.STM (atomically)
import Control.Concurrent.STM.TChan (TChan)

import Data.Map.Strict (insert, update, lookup)

import Data.Time.Clock (nominalDiffTimeToSeconds, diffUTCTime)
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)

import Control.Concurrent (threadDelay)

import Data.Fixed (Pico)

import GameEngineServer.AtomicTChan (readAllFromTChan)
import GameEngineServer.Network.TCPServer (ConnInfo (ConnInfo))
import GameEngineServer.Network.Packet (Packet (..), packetToPacketType)
import GameEngineServer.Mainloop.MainloopMessage (MainloopMessage (..), ReceiverMsgInner (..))
import GameEngineServer.Mainloop.MainloopClientApplicators (closeClient, sendPacketToClient)
import GameEngineServer.Config.Config (Config (..), CfgLevel (..))
import GameEngineServer.State.ServerState (ServerState (..), initialServerState)
import GameEngineServer.State.ClientApplicators (applyToAllClients, applyToClient)
import GameEngineServer.State.Client (Client (Client), getClientIdentifier, showClientMessage)
import GameEngineServer.SceneLoader.SceneLoader (loadScene)


-- |Handles inter-client communication and the server state
serverMainloop ::  TChan MainloopMessage -> Config -> IO ()
serverMainloop mainloopIn config = do
    sceneMaybe <- loadScene (cfglvlRoot $ cfgInitialScene config) (cfglvlFile $ cfgInitialScene config)
    case sceneMaybe of
        Just scene -> do
            putStrLn "Awaiting connections..."
            serverMainloopInner mainloopIn config ((initialServerState config) {ssScene = scene})
            putStrLn "Mainloop stopped"
        Nothing -> putStrLn "Couldn't load scene"

serverMainloopInner :: TChan MainloopMessage -> Config -> ServerState -> IO ()
serverMainloopInner mainloopIn config serverState = do
    startTime <- getSystemTime

    -- process all messages
    messages <- atomically $ readAllFromTChan mainloopIn
    newServerState <- foldr (\message serverStateIO -> serverStateIO >>= (mainloopMessageProcessor config message)) (return serverState) messages

    -- delay to keep a constant tick rate
    endTime <- getSystemTime
    let
        timeElapsed = diffUTCTime (systemToUTCTime endTime) (systemToUTCTime startTime)
        secondsElapsed = nominalDiffTimeToSeconds timeElapsed

        targetTime :: Pico
        targetTime = 1 / (fromInteger $ fromIntegral tickrate)

        secondsDelay :: Pico
        secondsDelay = targetTime - secondsElapsed

        secondsToMicroseconds :: Int
        secondsToMicroseconds = (10 :: Int) ^ (6 :: Int)

        microsecondsDelay :: Int
        microsecondsDelay = round (secondsDelay * (fromInteger $ fromIntegral secondsToMicroseconds))
    threadDelay microsecondsDelay

    serverMainloopInner mainloopIn config newServerState
    where
        tickrate = cfgTickrate config

-- |Process a single 'MainloopMessage'
mainloopMessageProcessor :: Config -> MainloopMessage -> ServerState -> IO ServerState
mainloopMessageProcessor config message serverState = do
    let
        ReceiverMsg connInfo msgInner = message
        ConnInfo uid _ = connInfo
    
    case msgInner of
        ClientConnEstablished connection -> do
            let clientActions = sendPacketToClient (ConnEstablished uid) <> sendPacketToClient (SetScene (cfglvlRoot $ ssCurrentLevel serverState) (cfglvlFile $ ssCurrentLevel serverState))
            Just newState <- applyToClient clientActions uid (serverState {ssClients = insert uid (Client connection connInfo Nothing) (ssClients serverState)})
            return newState
        
        _ -> case Data.Map.Strict.lookup uid (ssClients serverState) of
            Just client -> case msgInner of
                ClientConnEstablished _ -> error "This case should be handled above - this line only exists to silence warnings"

                PackedReceived packet -> handlePacket config serverState client packet
                
                ReceiverException errorMsg -> do
                    putStrLn $ showClientMessage client "listener threw an error - " ++ errorMsg
                    Just newServerState <- applyToClient closeClient uid serverState
                    return newServerState

                ClientConnClosed -> do
                    Just newServerState <- applyToClient closeClient uid serverState
                    return newServerState

            Nothing -> do
                putStrLn $ "Client UID " ++ show uid ++ " does not exist"
                return serverState

-- |Process a single 'Packet'
handlePacket :: Config -> ServerState -> Client -> Packet -> IO ServerState
handlePacket _ serverState client packet = case packet of
    ClientChatMessage strMessage -> do
        newServerState <- applyToAllClients (sendPacketToClient (ServerChatMessage (getClientIdentifier False client) strMessage)) serverState
        return newServerState
    
    SetClientName newName -> do
        putStrLn $ showClientMessage client ("name set to " ++ newName)
        return serverState {ssClients = update (\(Client connection connInfo _) -> Just (Client connection connInfo (Just newName))) uid (ssClients serverState)}
    
    _ -> do
        putStrLn $ showClientMessage client $ "client shouldn't send packet of type " ++ (show $ packetToPacketType packet)
        return serverState
    
    where
        Client _ (ConnInfo uid _) _ = client