module Main where

import Network.Socket
import Data.List.Split
import System.Exit
import System.Environment

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
        _ -> return() --process other messages here
    
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
