{-# LANGUAGE InstanceSigs #-}
module Main where

-- Sockets + Concurrency
import Network.Socket
import System.IO
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad.Fix
import Control.Monad (unless)

-- Appointment Data
import Data.UnixTime
import Data.ByteString.UTF8 (toString, fromString)
import Crypto.Hash.SHA256
import Data.ByteString.Base16 (encode, decode)

-- File IO
import System.Directory
import System.FilePath.Posix
import Data.List (findIndex, intercalate, find, isPrefixOf, dropWhileEnd, isInfixOf)
import Data.List.Split (splitOn, startsWith)
import GHC.IO (catchAny)
import Data.Char (isAscii, isSpace, toLower)
import Text.Printf (printf)


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

    welcome hdl
    -- authenticate hdl
    runSession hdl

    hClose hdl


runSession :: Handle -> IO ()
runSession hdl = (do
    input <- hGetLine hdl


    -- If they didn't type anything, or if they entered non-ascii characters, loop
    if (not . all isAscii) input then do putStrLn "INVALID CHAR INPUT" >> runSession hdl
    else do 

        -- Split input into args
        let args = words input

        if null args then do runSession hdl
        else unless (head args `elem` ["quit", "exit"]) $ do

            putStrLn $ "Command: <" ++ input ++ ">" -- Debug

            let cmd = lookup (head args) dispatch

            case cmd of
                Just f -> f hdl args
                Nothing -> hPutStr hdl "Unknown command" >> help hdl args

            -- Loop
            runSession hdl

    ) `catchAny` (\e -> do -- Keep session alive on error
        hPutStrLn hdl $ "Error: " ++ show e
        runSession hdl
    )


----------------- COMMANDS -----------------

authenticate :: Handle -> IO ()
authenticate hdl = do
    hPutStrLn hdl "Please enter your name: "
    name <- hGetLine hdl
    hPutStrLn hdl $ "Welcome, " ++ name ++ "!"

welcome :: Handle -> IO ()
welcome = flip hPutStrLn "\ESC[34mWelcome!\ESC[0m\n";

dispatch :: [(String, Handle -> [String] -> IO ())]
dispatch = [ ("help",       help)
           , ("list",       list)
           , ("search",     search)
           , ("clear",      clear)
           , ("add",        add)
           , ("remove",     remove)
           , ("next",       next)
           , ("timenow",    timenow)
           ]

help :: Handle -> [String] -> IO ()
help hdl _ = hPutStrLn hdl
  "\n\ESC[1;34mAppointments Manager\ESC[0m\n\
  \-----------------------\n\
  \\ESC[36m\
  \help     - show this message\n\
  \search   - search appointments\n\
  \list     - list all appointments\n\
  \clear    - clear the screen\n\
  \add      - add a new appointment\n\
  \remove   - remove appoint\n\
  \next     - show the next scheduled appointment\n\
  \timenow  - show the current date and time\n\
  \quit     - exit the session\n\
  \\ESC[0m"

search :: Handle -> [String] -> IO ()
search hdl _ = do
    appts <- getAppointments
    hPutStrLn hdl "Enter search term:"
    term <- hGetLine hdl

    let stringAppts = map printAppointment appts
        matches = filter (\x -> (map toLower . trim) term `isInfixOf` map toLower x) stringAppts

    case matches of
        [] -> hPutStrLn hdl "\ESC[31mNo matches found\ESC[0m"
        _ -> do
            hPutStrLn hdl "\ESC[1;32mMatches\ESC[0m:"
            mapM_ (hPutStrLn hdl) matches


clear :: Handle -> [String] -> IO ()
clear hdl _ = hPutStrLn hdl "\ESC[2J\n"

list :: Handle -> [String] -> IO ()
list hdl _ = do
    appts <- getAppointments
    case appts of
        [] -> hPutStrLn hdl "\ESC[31mYou don't have any appointments yet!\ESC[0m"
        _ ->  do
                hPutStrLn hdl "\ESC[1;32mYour appointments\ESC[0m:"
                -- mapM_ (hPrint hdl) appts
                mapM_ (hPutStrLn hdl . printAppointment) appts

add :: Handle -> [String] -> IO ()
add hdl _ = do
    hPutStrLn hdl "Enter time [hh:mm|dd/mm/yyyy]:"
    time <- hGetLine hdl
    hPutStrLn hdl "Enter location:"
    loc <- hGetLine hdl
    hPutStrLn hdl "Enter description:"
    desc <- hGetLine hdl

    case createAppointment $ map trim [time, loc, desc] of
        Nothing -> hPutStrLn hdl "\ESC[31mInvalid appointment\ESC[0m"
        Just appointment -> do
            let hash = hashAppointment appointment
            createAndWriteFile (appointDir ++ hash) (show appointment)
            hPutStrLn hdl $ "Appointment " ++ hash ++ " created"


createAndWriteFile :: FilePath -> String -> IO ()
createAndWriteFile path content = do
    createDirectoryIfMissing True (takeDirectory path)
    writeFile path content

remove :: Handle -> [String] -> IO ()
remove hdl _ = do
    hPutStrLn hdl "Remove by [i(d)/t(ime)]:"
    remBy <- hGetLine hdl

    case remBy of
        -- Remove by id
        'i':_ -> do
            hPutStrLn hdl "Enter start of appointment id:"
            aid <- hGetLine hdl -- May only be starting characters
            appts <- getAppointmentIds

            case find (trim aid `isPrefixOf`) appts of
                Just hash -> do
                    removeFile (appointDir ++ hash)
                    hPutStrLn hdl $ "Appointment " ++ hash ++ " removed"

                Nothing -> hPutStrLn hdl "\ESC[31mNo appointment found\ESC[0m"

        -- Remove by time
        't':_ -> do
            hPutStrLn hdl "Enter time of appointment [hh:mm|dd/mm/yyyy]:"
            atime <- hGetLine hdl
            appts <- getAppointments

            let atime' = parseTime atime

            case atime' of
                Null -> hPutStrLn hdl "\ESC[31mInvalid time\ESC[0m"
                _ -> case find (\Appointment {time = t} -> t == atime') appts of
                    Just all@Appointment {time = t} -> do
                            let hash = hashAppointment all
                            removeFile (appointDir ++ hash)
                            hPutStrLn hdl $ "Appointment " ++ hash ++ " removed"

                    Nothing -> hPutStrLn hdl "\ESC[31mNo appointment found\ESC[0m"

        _ -> hPutStrLn hdl "\ESC[31mInvalid option\ESC[0m"

next :: Handle -> [String] -> IO ()
next hdl _ = do
    appts <- getAppointments

    unixTimeNow <- getUnixTime
    fnow <- formatUnixTime (fromString "%H:%M|%d/%m/%Y") unixTimeNow

    now <- case parseTime $ toString fnow of
        Null -> hPutStrLn hdl "Something went wrong while getting the current time"
        x -> do
            case find (\Appointment {time = t} -> t <= x) appts of
                Just appt -> hPutStrLn hdl $ "\ESC[33mNext appointment\ESC[0m: " ++ printAppointment appt
                Nothing -> hPutStrLn hdl "\ESC[31mNo upcoming appointments\ESC[0m"

    return ()

timenow :: Handle -> [String] -> IO ()
timenow hdl _ = do
    appts <- getAppointments

    unixTimeNow <- getUnixTime
    fnow <- formatUnixTime (fromString "%H:%M|%d/%m/%Y") unixTimeNow

    now <- case parseTime $ toString fnow of
        Null -> hPutStrLn hdl "Something went wrong while getting the current time"
        x -> hPutStrLn hdl $ "\ESC[1;35m" ++ printTime x ++ "\ESC[0m"
    
    return ()


----------------- APPOINTMENTS -----------------
-- Add notes to appointments
-- Hash appointments for unique id --> No duplicate appointments
-- Store each appointment in its own file

data Appointment = Appointment  { time :: Time
                                , location :: String
                                , description :: String
                                } deriving (Show, Read)

printAppointment :: Appointment -> String
printAppointment appt@Appointment {time = t, location = l, description = d} = intercalate " |\t" $ map show [hashAppointment appt, printTime t, _l, _d]
    where
        _l = if null l then "No location" else l
        _d = if null d then "No description" else d

appointDir :: FilePath
appointDir = "appointments/"

createAppointment :: [String] -> Maybe Appointment
createAppointment [time, loc, desc] =   case t of
                                            Null -> Nothing
                                            t -> Just $ Appointment t loc desc
                                        where t = parseTime time
createAppointment _ = Nothing

-- Hash an appointment to 16 char string
hashAppointment :: Appointment -> String
hashAppointment = take 16 . toString . encode . hash . fromString . show

getAppointmentIds :: IO [String]
getAppointmentIds = do
    dirExists <- doesDirectoryExist appointDir

    if not dirExists
    then do
        createDirectory appointDir
        return []
    else do
        getDirectoryContents appointDir

getAppointments :: IO [Appointment]
getAppointments = do
    dirExists <- doesDirectoryExist appointDir

    if not dirExists
    then do
        createDirectory appointDir
        return []
    else do
        files <- getDirectoryContents appointDir
        let appts = filter (`notElem` [".", ".."]) files
        let x = mapM (readFile . (appointDir ++)) appts
        contents <- x
        let res = map read contents :: [Appointment]
        return res

----------------- TIME -----------------

data Time = Time    { day :: Int
                    , month :: Int
                    , year :: Int
                    , hour :: Int
                    , minute :: Int
                    }
            | Null  deriving (Show, Read)

instance Eq Time where
    (==) (Time d1 m1 y1 h1 mi1) (Time d2 m2 y2 h2 mi2) = d1 == d2 && m1 == m2 && y1 == y2 && h1 == h2 && mi1 == mi2
    (==) _ _ = False

instance Ord Time where
    compare (Time d1 m1 y1 h1 mi1) (Time d2 m2 y2 h2 mi2) = compare (val y1 m1 d1 h1 mi1) (val y2 m2 d2 h2 mi2)
        where val d m y h mi = (((y*12+m)*31+d)*24+h)*60+mi
    compare _ _ = EQ


-- Check if a given date would actually exist
isDateValid :: Int -> Int -> Int -> Bool
isDateValid day month year
    | day < 1                         = False
    | (==) month 2                    = day <= if isLeapYear then 29 else 28
    | elem month [4,6,9,11]           = day <= 30
    | elem month [1,3,5,7,8,10,12]    = day <= 31
    | otherwise                       = False
    where isLeapYear = year `mod` 400 == 0 || (year `mod` 100 /= 0 && year `mod` 4 == 0)

isTimeValid :: Int -> Int -> Bool
isTimeValid hour minute = hour >= 0 && hour < 24 && minute >= 0 && minute < 60

parseTime :: String -> Time
parseTime time =    if isDateValid day month year && isTimeValid hour minute
                        then Time day month year hour minute
                        else Null
                    where
                        parts = splitOn "|" time
                        dt = splitOn "/" $ last parts
                        tm = splitOn ":" $ head parts

                        [day, month, year] =    if length dt == 3 then map read dt else [-1, -1, -1]
                        [hour, minute] =        if length tm == 2 then map read tm else [-1, -1]

printTime :: Time -> String
-- printTime Time {day = d, month = m, year = y, hour = h, minute = min} = show h ++ ":" ++ show min ++ "-" ++ show d ++ "/" ++ show m ++ "/" ++ show y
printTime Time {day = d, month = m, year = y, hour = h, minute = min}
    =   foldr (\(a, b, c) acc -> printf c a ++ b ++ acc) ""
        [(h, ":", "%02d"), (min, "-", "%02d"), (d, "/", "%02d"), (m, "/", "%02d"), (y, "", "%04d")]

trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace