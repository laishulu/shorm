{-# LANGUAGE ScopedTypeVariables #-}

module Shorm.Commands
  ( add
  , list
  , delete
  , edit
  , search
  , version
  , backupClean
  , backupRestore
  , backupList
  , backupCreate
  , getSSHConfigPath
  )
where

import           System.Directory               ( listDirectory
                                                , removeFile
                                                , copyFile
                                                )
import           System.FilePath                ( (</>)
                                                , takeFileName
                                                , takeDirectory
                                                )
import           Data.List                      ( sortOn )
import           Data.Ord                       ( Down(Down) )
import           Data.List                      ( find
                                                , isInfixOf
                                                )
import           System.Directory               ( getHomeDirectory
                                                , doesFileExist
                                                )
import           Data.Char                      ( toLower )
import           Control.Exception              ( try
                                                , IOException
                                                , SomeException
                                                , fromException
                                                , throwIO
                                                )
import           System.Timeout                 ( timeout )
import           System.IO.Error                ( ioeGetErrorString )
import           Text.Read                      ( readMaybe )
import           Data.Maybe                     ( fromMaybe, isJust )
import           Data.Time.Clock.POSIX          ( POSIXTime
                                                , getPOSIXTime
                                                , posixSecondsToUTCTime
                                                )
import           Data.Time.Format               ( formatTime
                                                , defaultTimeLocale
                                                )

-- Use Config.SSHConnection instead of defining a new type
import qualified Shorm.Config as Config

getSSHConfigPath :: IO FilePath
getSSHConfigPath = do
  home <- getHomeDirectory
  return $ home </> ".ssh/config"

add :: String -> String -> Maybe String -> IO ()
add connName userHostPort maybeIdFile = do
  configPath <- getSSHConfigPath
  -- Create a backup before adding a new connection
  backupCreate configPath
  let (connUser, connHost, connPort) = parseUserHostPort userHostPort
      newConnection      = Config.normalizeConnection $ Config.SSHConnection
        { Config.name            = connName
        , Config.host            = connHost
        , Config.user            = connUser
        , Config.port            = Just $ fromMaybe 22 connPort
        , Config.idFile          = maybeIdFile
        , Config.identitiesOnly  = isJust maybeIdFile
        }
  connections <- Config.readConfig configPath
  let existingConnections = filter (\conn -> Config.name conn == connName) connections
  if not (null existingConnections)
    then do
      putStrLn $ "Error: A connection with the name '" ++ connName ++ "' already exists."
      putStrLn "Please choose a different name or use the 'edit' command to modify the existing connection."
    else do
      result <- withTimeout 30000000 $ try $ Config.appendConnection configPath newConnection
      handleAddResult result newConnection

findExistingConnections :: FilePath -> String -> IO [Config.SSHConnection]
findExistingConnections configPath connName = do
  connections <- Config.readConfig configPath
  return $ filter (\conn -> Config.name conn == connName) connections

handleAddResult
  :: Maybe (Either SomeException (Either String ()))
  -> Config.SSHConnection
  -> IO ()
handleAddResult result conn = case result of
  Nothing                       -> handleTimeout
  Just (Left  err             ) -> handleError err
  Just (Right (Left  errorMsg)) -> handleConfigError errorMsg
  Just (Right (Right ()      )) -> handleSuccess conn

handleConfigError :: String -> IO ()
handleConfigError "Connection already exists" =
  putStrLn "Error: A connection with this name already exists."
handleConfigError errorMsg = do
  putStrLn $ "Error adding connection: " ++ errorMsg
  putStrLn "This might be due to invalid configuration data or a parsing error."

handleTimeout :: IO ()
handleTimeout = do
  putStrLn "Operation timed out after 30 seconds. This could be due to:"
  putStrLn "  - The config file being inaccessible"
  putStrLn "  - The system being under heavy load"
  putStrLn "  - Network issues if the file is on a remote system"
  putStrLn
    "Please check the file permissions and system resources, then try again."

handleError :: SomeException -> IO ()
handleError err = do
  putStrLn $ "Error adding connection: " ++ show err
  if isIOException err
    then do
      putStrLn "I/O error occurred. This could be due to:"
      putStrLn "  - Insufficient permissions"
      putStrLn "  - Disk full"
      putStrLn "  - File system issues"
    else putStrLn "An unexpected error occurred."
  putStrLn "Please check file permissions and system resources, then try again."

handleSuccess :: Config.SSHConnection -> IO ()
handleSuccess conn = printConnection conn

list :: IO ()
list = do
  configPath <- getSSHConfigPath
  exists     <- doesFileExist configPath
  if not exists
    then putStrLn $ "SSH config file not found at: " ++ configPath
    else do
      result <- timeout 5000000 $ try (Config.readConfig configPath)
      handleListResult result

handleListResult :: Maybe (Either IOException [Config.SSHConnection]) -> IO ()
handleListResult result = case result of
  Nothing -> putStrLn
    "Operation timed out. The config file might be too large or inaccessible."
  Just (Left err) -> putStrLn $ "Error reading config file: " ++ show err
  Just (Right connections) -> if null connections
    then putStrLn "No connections found in the config file."
    else mapM_ printConnection connections

printConnection :: Config.SSHConnection -> IO ()
printConnection conn =
  putStrLn
    $  Config.name conn
    ++ " -> "
    ++ (maybe "" (++ "@") (Config.user conn))
    ++ Config.host conn
    ++ (maybe ""
              (\p -> if p /= 22 then ":" ++ show p else "")
              (Config.port conn)
       )
    ++ (maybe "" (\connIdFile -> " (ID: " ++ connIdFile ++ (if Config.identitiesOnly conn then ", IdentitiesOnly" else "") ++ ")") (Config.idFile conn))

delete :: String -> IO ()
delete connName = do
  configPath <- getSSHConfigPath
  
  result <- try $ do
    connections <- Config.readConfig configPath
    case find (\conn -> Config.name conn == connName) connections of
      Nothing -> throwIO $ userError $ "Connection not found: " ++ connName
      Just _ -> do
        let updatedConnections = filter (\conn -> Config.name conn /= connName) connections
        Config.writeConfig configPath updatedConnections

  case result of
    Left e -> case fromException e of
      Just (err :: IOError) -> putStrLn $ "Error: " ++ ioeGetErrorString err
      _ -> putStrLn $ "Error deleting connection: " ++ show e
    Right _ -> putStrLn $ "Successfully deleted connection: " ++ connName

edit :: String -> String -> Maybe String -> IO ()
edit connName userHostPort maybeIdFile = do
  configPath <- getSSHConfigPath
  -- Create a backup before editing a connection
  backupCreate configPath
  result <- try $ do
    connections <- Config.readConfig configPath
    case find (\conn -> Config.name conn == connName) connections of
      Nothing -> throwIO $ userError $ "Connection not found: " ++ connName
      Just oldConn -> do
        let (editUser, editHost, editPort) = parseUserHostPort userHostPort
        let editIdFile = maybeIdFile `orElse` Config.idFile oldConn
        let editedConn = oldConn { Config.host = editHost
                                 , Config.user = editUser
                                 , Config.port = editPort
                                 , Config.idFile = editIdFile
                                 , Config.identitiesOnly = isJust editIdFile
                                 }
        let updatedConnections = map (\conn -> if Config.name conn == connName then editedConn else conn) connections
        Config.writeConfig configPath updatedConnections
        return editedConn
  case result of
    Left e -> case fromException e of
      Just (err :: IOError) -> putStrLn $ "Error: " ++ ioeGetErrorString err
      _ -> do
        putStrLn $ "Error editing connection: " ++ show e
        putStrLn "There might be a problem with the config file or permissions."
    Right editedConn -> do
      putStrLn $ "Successfully edited connection: " ++ connName
      printConnection editedConn

-- Helper function to use the first Just value
orElse :: Maybe a -> Maybe a -> Maybe a
orElse (Just x) _ = Just x
orElse Nothing y = y

parseUserHostPort :: String -> (Maybe String, String, Maybe Int)
parseUserHostPort input = case break (== '@') input of
  (userPart, '@' : hostPortPart) ->
    let (hostPart, portPart) = parseHostPort hostPortPart in (Just userPart, hostPart, portPart)
  (hostPortPart, "") ->
    let (hostPart, portPart) = parseHostPort hostPortPart in (Nothing, hostPart, portPart)
  (userPart, _) ->
    -- This case handles any unexpected input
    (Just userPart, "", Nothing)

parseHostPort :: String -> (String, Maybe Int)
parseHostPort hostPortPart = case break (== ':') hostPortPart of
  (hostPart, ':' : portStr) -> case readMaybe portStr of
    Just portNum -> (hostPart, Just portNum)
    Nothing   -> (hostPart, Nothing)
  (hostPart, "") -> (hostPart, Nothing)
  (hostPart, _) ->
    -- This case handles any unexpected input
    (hostPart, Nothing)

search :: String -> IO ()
search query = do
  configPath  <- getSSHConfigPath
  connections <- Config.readConfig configPath
  let matches = filter (matchesQuery (map toLower query)) connections
  printSearchResults matches

matchesQuery :: String -> Config.SSHConnection -> Bool
matchesQuery q conn = any
  (matchesSubstring q)
  [ map toLower (Config.name conn)
  , map toLower (Config.host conn)
  , maybe "" (map toLower) (Config.user conn)
  ]

matchesSubstring :: String -> String -> Bool
matchesSubstring needle haystack =
  let wordBoundaries = "." : words haystack
  in  any (needle `isInfixOf`) wordBoundaries

printSearchResults :: [Config.SSHConnection] -> IO ()
printSearchResults matches = if null matches
  then putStrLn "No connections found matching the search query."
  else mapM_ printConnection matches
withTimeout :: Int -> IO a -> IO (Maybe a)
withTimeout microseconds action = timeout microseconds action

isIOException :: SomeException -> Bool
isIOException e = case fromException e of
  Just (_ :: IOException) -> True
  _                       -> False

version :: IO ()
version = putStrLn $ "shorm version " ++ version_name
  where
    version_name = "1.1.1" -- automatically updated by bump-version.sh


backupClean :: FilePath -> IO ()
backupClean configPath = do
  let configDir = takeDirectory configPath
  files <- listDirectory configDir
  let backups = filter (isBackupFile $ takeFileName configPath) files
  mapM_
    (\backup -> do
      removeFile (configDir </> backup)
      putStrLn $ "Removed backup: " ++ backup
    )
    backups
  putStrLn $ "Removed " ++ show (length backups) ++ " backup files."

backupRestore :: FilePath -> Maybe String -> IO ()
backupRestore configPath maybeBackupName = do
  let configDir = takeDirectory configPath
  files <- listDirectory configDir
  let backups = filter (isBackupFile $ takeFileName configPath) files
  case maybeBackupName of
    Just backupName -> if backupName `elem` backups
      then do
        copyFile (configDir </> backupName) configPath
        putStrLn $ "Restored backup: " ++ backupName
      else putStrLn $ "Backup not found: " ++ backupName
    Nothing -> case sortOn (Down . getTimestamp) backups of
      (latestBackup : _) -> do
        copyFile (configDir </> latestBackup) configPath
        putStrLn $ "Restored latest backup: " ++ latestBackup
      [] -> putStrLn "No backups found."

backupList :: FilePath -> IO ()
backupList configPath = do
  let configDir = takeDirectory configPath
  files <- listDirectory configDir
  let backups = filter (isBackupFile $ takeFileName configPath) files
  if null backups
    then putStrLn "No backups found."
    else do
      putStrLn "Available backups:"
      mapM_ (\backup -> putStrLn $ "  " ++ backup)
        $ sortOn (Down . getTimestamp) backups

isBackupFile :: String -> String -> Bool
isBackupFile configName fileName =
  take (length configName) fileName
    == configName
    && length (drop (length configName) fileName)
    == 16
    &&  -- "-YYYYMMDD-HHMMSS"
       all (\c -> c == '-' || c `elem` ['0' .. '9'])
           (drop (length configName) fileName)

getTimestamp :: String -> String
getTimestamp = takeWhile (/= '.') . drop 1 . dropWhile (/= '.')

backupCreate :: FilePath -> IO ()
backupCreate configPath = do
  currentTime <- getPOSIXTime
  let timestamp  = formatTimestamp currentTime
      backupName = "config-" ++ timestamp
      backupPath = takeDirectory configPath </> backupName
  copyFile configPath backupPath
  putStrLn $ "Created backup: " ++ backupPath

formatTimestamp :: POSIXTime -> String
formatTimestamp =
  formatTime defaultTimeLocale "%Y%m%d-%H%M%S" . posixSecondsToUTCTime
