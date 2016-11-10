module Main where

import Network.Socket
import Data.List.Split
import System.Exit

main :: IO ()
main = do
    --create a TCP Socket
    sock <- socket AF_INET Stream 0

    setSocketOption sock ReuseAddr 1

    --bind the socket to port 4242
    bind sock (SockAddrInet 4242 iNADDR_ANY) 

    --listen for at most 2 queued connections
    listen sock 2

    mainLoop sock

mainLoop :: Socket -> IO ()
mainLoop sock = do
    --accept a connection
    conn <- accept sock

    runConn conn   

    --recurse
    --mainLoop sock

runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, sockAddr) = do
    msg <- recv sock 1024
    let split = words msg
    case head split of
        "KILL_SERVICE" -> killCon sock
        "HELO" -> processHelo (sock, sockAddr, msg)
        _ -> return()
    
    runConn(sock, sockAddr)

killCon :: (Socket) -> IO ()
killCon (sock) = do
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