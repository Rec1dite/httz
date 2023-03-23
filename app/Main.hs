{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Sockets + Concurrency
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan

-- Template Parsing
import Text.Mustache
import Text.Mustache.Types
import Data.Text (unpack)

-- Appointment Data
import Data.UnixTime
import Data.ByteString.UTF8 (toString, fromString)
import GHC.IO (catchAny)


type Msg = String

----------------- IO -----------------
main :: IO ()
main = do
    sock <- socket AF_INET Stream 0             -- Create socket
    setSocketOption sock ReuseAddr 1            -- Make socket immediately reusable - eases debugging.
    bind sock (SockAddrInet 4000 0)             -- Listen on TCP port 4000
    listen sock 2                               -- Allow a maximum of 2 queued connections

    chan <- newChan                             -- Create a new message channel
    mainLoop sock chan

-- The server loop which listens for new connections
mainLoop :: Socket -> Chan Msg -> IO ()
mainLoop sock chan = do
    conn <- accept sock                        -- Accept a connection and handle it
    forkIO $ runConn conn                      -- Fork to a new thread to handle the connection
    mainLoop sock chan

-- Handle a single connection
runConn :: (Socket, SockAddr) -> IO ()
runConn (sock, _) = do
    hdl <- socketToHandle sock ReadWriteMode    -- Convert socket to a handle
    hSetBuffering hdl NoBuffering               -- Set buffering mode

    input <- hGetContents hdl

    if (head . words) input == "GET" then do
        -- Generate from page template
        let searchspace = ["./content"]
            template = "index.html"


        compiled <- automaticCompile searchspace template
        case compiled of
            Left parseErr -> putStrLn $ "Error: " ++ show parseErr
            Right template -> do

                -- Get current time
                unixTimeNow <- getUnixTime
                sTime <- formatUnixTime timeFormat unixTimeNow
                sDate <- formatUnixTime dateFormat unixTimeNow

                let context = object
                        [ "time-time" ~> toString sTime
                        , "time-date" ~> toString sDate
                        ]

                    rendered = substituteValue template context

                hPutStr hdl $ (responsify . unpack) rendered

    else do hPutStrLn hdl "HTTP/1.1 404 Not Found\r\n\r\n"

    hClose hdl

timeFormat :: Format
timeFormat = fromString "%H:%M"

dateFormat :: Format
dateFormat = fromString "%d/%m/%Y"

responsify :: String -> String
responsify s = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" ++ s