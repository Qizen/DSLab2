module Main where

import Network.Socket
import Data.List.Split
import System.Exit
import System.Environment
import Control.Concurrent

type Msg = String

main :: IO ()
main = do
    --read port number from command line
    args <- getArgs
    let port = args !! 0

    --create a TCP Socket
    sock <- socket AF_INET Stream 0

    setSocketOption sock ReuseAddr 1

    --bind the socket to port 
    bind sock (SockAddrInet (fromIntegral(read(port))) iNADDR_ANY) 

    --listen for at most 2 queued connections
    listen sock 2

    --create a new FIFO channel for threads to communicate
    chan <- newChan

    forkIO $ connDispatchLoop sock chan
    
    mainLoop sock chan

mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    message <- readChan chan
    case message of 
        "KILL" -> killCon sock
        _ -> mainLoop sock chan

connDispatchLoop :: Socket -> Chan Msg -> IO ()
connDispatchLoop sock chan = do
    --accept a connection
    conn <- accept sock

    forkIO $ runConn conn chan   

    --recurse
    connDispatchLoop sock chan

runConn :: (Socket, SockAddr) -> Chan Msg -> IO ()
runConn (sock, sockAddr) chan = do
    msg <- recv sock 1024
    let split = words msg
    case head split of
        "KILL_SERVICE" -> writeChan chan "KILL"
        "HELO" -> processHelo (sock, sockAddr, msg)
        _ -> return () --process other messages here
    
    runConn (sock, sockAddr) chan

killCon :: Socket -> IO ()
killCon sock = do
    putStrLn "Killing the Service"
    close sock
    exitSuccess

processHelo :: (Socket, SockAddr, String) -> IO ()
processHelo (sock, sockAddr, msg) = do
    let split = words msg 
    let addr = splitOn ":" (show sockAddr)
    let ip = addr !! 0
    let port = addr !! 1
    send sock $ "HELO " ++ split !! 1 ++ "\n\
                \IP:" ++ ip ++ "\n\
                \Port:" ++ port ++ "\n\
                \StudentID:12312907\n"
    return ()                              
