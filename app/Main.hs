{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}
module Main where

-- Sockets + Concurrency
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan

-- Template Parsing
import Text.Mustache
import Text.Mustache.Types
import Data.Text (unpack, split, pack, isPrefixOf, isInfixOf, toLower)

-- Appointment Data
import Data.UnixTime
import Data.ByteString.UTF8 (toString, fromString)
import GHC.IO (catchAny)
import Network.Socket.ByteString (sendAll, recv)
import qualified Data.ByteString.Lazy.Char8 as LBS

import Data.Time.Zones.DB
import Data.Time.Zones.Read
import Data.Time.Clock
import Data.Time.Zones (timeZoneForPOSIX, utcToLocalTimeTZ)


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

    case (head . words) input of
        "GET" -> do

            -- Log request
            let request = (head . tail . words) input
            print request

            case takeWhile (/= '?') request of
                "/" -> getTzPage hdl getAllTimeZones
                "/search" -> do
                    let query = concatMap unpack . split (== '+') . pack $ reverse . takeWhile (/= '=') . reverse $ request

                    print $ "Q: " ++ query

                    let tzs = filter checkMatch getAllTimeZones
                        checkMatch = isInfixOf (toLower $ pack query) . toLower . pack . show

                    getTzPage hdl tzs

                _ -> hPutStrLn hdl "HTTP/1.1 404 Not Found\r\n\r\n"

        _ -> hPutStrLn hdl "HTTP/1.1 404 Not Found\r\n\r\n"

    hClose hdl

getAllTimeZones :: [TZLabel]
getAllTimeZones = [minBound .. maxBound]

-- tzToDiv :: TZLabel -> String
-- tzToDiv tz = "<div class=\"box\">" ++ (encode tzDataByLabel tz) ++ "</div>"

timeFormat :: Format
timeFormat = fromString "%H:%M"

dateFormat :: Format
dateFormat = fromString "%d/%m/%Y"

responsify :: String -> String
responsify s = "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\n\r\n" ++ s

getTzPage :: Handle -> [TZLabel] -> IO ()
getTzPage hdl timezones = do

    -- Generate from page template
    let searchspace = ["./content"]
        template = "index.html"

    compiled <- automaticCompile searchspace template
    case compiled of
        Left parseErr -> putStrLn $ "Error: " ++ show parseErr
        Right template -> do

            -- Get current time
            unixTimeNow <- getUnixTime

            now <- getCurrentTime

            let names = map show timezones
            -- times <- mapM (\tz -> formatUnixTime timeFormat unixTimeNow) timezones
            let locTimes = map ((`utcToLocalTimeTZ` now) . parseOlson . tzDataByLabel) timezones
            let dates = map (head . words . show) locTimes
            let times = map (take 8 . head . tail . words . show) locTimes

            -- dates <- mapM (\tz -> formatUnixTime dateFormat unixTimeNow) timezones

            let tzs = map (\(n, t, d) -> TimeZone n t d) $ zip3 names times dates
            -- let tzs = map show [1..10]

            let context = object [ "timezones" ~> tzs ]

                rendered = substituteValue template context

            hPutStr hdl $ (responsify . unpack) rendered

data TimeZone = TimeZone { tzname :: String
                         , tztime :: String
                         , tzdate :: String
                         }

instance ToMustache TimeZone where
    toMustache :: TimeZone -> Value
    toMustache tz = object
        [ "name" ~> tzname tz
        , "time" ~> tztime tz
        , "date" ~> tzdate tz
        ]