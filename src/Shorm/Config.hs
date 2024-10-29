{-# LANGUAGE ScopedTypeVariables #-}

module Shorm.Config
  ( SSHConnection(..)
  , readConfig
  , normalizeConnection
  , writeConfig
  , appendConnection
  , removeConnection
  , updateConnection
  )
where

import           System.Directory               ( doesFileExist
                                                , removeFile
                                                , renameFile
                                                , createDirectoryIfMissing
                                                , copyFile
                                                )
import           Control.Exception              ( try
                                                , IOException
                                                , bracket
                                                , catch
                                                , throwIO
                                                )
import           System.IO.Error                ( isDoesNotExistError )
import           System.FilePath                ( takeDirectory, takeFileName, (</>), takeExtension )
import           Data.Time.Clock                ( getCurrentTime )
import           Data.Time.Format               ( formatTime, defaultTimeLocale )
import           System.IO                      ( hPutStr
                                                , hClose
                                                , openTempFile
                                                , Handle
                                                )
import           Data.Char                      ( toLower
                                                , isSpace
                                                )
import           Data.Maybe                     ( mapMaybe
                                                , listToMaybe
                                                , fromMaybe
                                                )
import           Text.Read                      ( readMaybe )
import           Data.List                      ( nubBy
                                                , groupBy
                                                )
import           Text.Regex.TDFA                ( (=~) )
import           Control.Monad                  ( when )

data SSHConnection = SSHConnection
  { name :: String
  , host :: String
  , user :: Maybe String
  , port :: Maybe Int
  } deriving (Show, Eq)

formatConnection :: SSHConnection -> String
formatConnection conn =
  unlines
    $  ["Host " ++ name conn, "    HostName " ++ host conn]
    ++ (maybe [] (\u -> ["    User " ++ u]) (user conn))
    ++ (case port conn of
         Just p | p /= 22 -> ["    Port " ++ show p]
         _                -> []
       )
    ++ [""]  -- Empty line between entries


normalizeConnection :: SSHConnection -> SSHConnection
normalizeConnection conn = conn { host = map toLower (host conn)
                                , user = fmap (map toLower) (user conn)
                                , port = port conn
                                }


strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

readConfig :: FilePath -> IO [SSHConnection]
readConfig path = do
  exists <- doesFileExist path
  if exists
    then do
      content <- readFile path
      let blocks            = splitOnHost $ lines content
          connections       = mapMaybe parseConnection blocks
          uniqueConnections = nubBy isSameConnection connections
      return uniqueConnections
    else return []
 where
  splitOnHost =
    filter (not . null) . groupBy (\_ y -> not (isHostLine y)) . filter
      (not . null . strip)

  isSameConnection conn1 conn2 =
    map toLower (host conn1) == map toLower (host conn2)
      && (fmap (map toLower) (user conn1) == fmap (map toLower) (user conn2))
      && map toLower (name conn1) == map toLower (name conn2)

parseConnection :: [String] -> Maybe SSHConnection
parseConnection configLines = do
  hostLine <- listToMaybe $ filter isHostLine $ map strip configLines
  let name' =
        (hostLine :: String)
          =~ ("^[Hh]ost[[:space:]]+([^[:space:]]+)" :: String) :: [[String]]
      name'' = case name' of
        ((_ : match : _) : _) -> strip match
        _                     -> ""

      getField pattern = listToMaybe
        [ strip match
        | line <- configLines
        , let matches = (line :: String) =~ (pattern :: String) :: [[String]]
        , not (null matches)
        , (_ : match : _) <- matches
        ]

  if null name''
    then Nothing
    else Just $ normalizeConnection $ SSHConnection
      name''
      ( fromMaybe name''
      $ getField "^[[:space:]]*[Hh]ost[Nn]ame[[:space:]]+([^[:space:]]+)"
      )
      (getField "^[[:space:]]*[Uu]ser[[:space:]]+([^[:space:]]+)")
      (getField "^[[:space:]]*[Pp]ort[[:space:]]+([0-9]+)" >>= readMaybe)

isHostLine :: String -> Bool
isHostLine line =
  (line :: String) =~ ("^[Hh]ost[[:space:]]+" :: String) :: Bool

withTempFile :: FilePath -> String -> (FilePath -> Handle -> IO a) -> IO a
withTempFile dir template action = bracket
  (openTempFile dir template)
  (\(tempPath, tempHandle) -> do
    hClose tempHandle
    removeFile tempPath `catch` \e -> if isDoesNotExistError e
      then return ()  -- If the file doesn't exist, just continue
      else throwIO e  -- Re-throw any other kind of error
  )
  (\(tempPath, tempHandle) -> action tempPath tempHandle)

writeConfig :: FilePath -> [SSHConnection] -> IO ()
writeConfig path conns = do
  backupPath <- generateBackupFilename path
  exists <- doesFileExist path
  when exists $ do
    copyFile path backupPath
    putStrLn $ "Backup created: " ++ backupPath
  withTempFile "." "shorm_temp_config" $ \tempPath tempHandle -> do
    mapM_ (hPutStr tempHandle . formatConnection) conns
    hClose tempHandle
    renameFile tempPath path

appendConnection :: FilePath -> SSHConnection -> IO (Either String ())
appendConnection path conn = do
  -- Ensure parent directory exists
  createDirectoryIfMissing True (takeDirectory path)

  result <- try $ do
    existing <- readConfig path
    let updatedConnections = existing ++ [conn]
    writeConfig path updatedConnections

  case result of
    Left (e :: IOException) ->
      return $ Left $ "Failed to update config: " ++ show e
    Right _ -> return $ Right ()

removeConnection :: FilePath -> String -> IO ()
removeConnection path name' = do
  existing <- readConfig path
  writeConfig path (filter (\conn -> name conn /= name') existing)

updateConnection :: FilePath -> SSHConnection -> IO ()
updateConnection path newConn = do
  existing <- readConfig path
  let updated = map
        (\conn -> if name conn == name newConn then newConn else conn)
        existing
  writeConfig path updated
generateBackupFilename :: FilePath -> IO FilePath
generateBackupFilename path = do
    now <- getCurrentTime
    let timestamp = formatTime defaultTimeLocale "%Y%m%d-%H%M%S" now
        (dir, file) = (takeDirectory path, takeFileName path)
        ext = takeExtension file
        nameWithoutExt = take (length file - length ext) file
        backupName = nameWithoutExt ++ "-" ++ timestamp ++ ext
    return $ dir </> backupName
