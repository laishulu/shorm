module Main
  ( main
  )
where

import qualified Shorm.Commands                as Commands
import           Options.Applicative

data Command
  = Add String String (Maybe String)  -- name user@host[:port] id_file
  | List
  | Delete String
  | Edit String String (Maybe String)  -- name userHost id_file
  | Search String
  | Version
  | BackupClean
  | BackupRestore (Maybe String)
  | BackupList
  | BackupCreate
  | BackupHelp

commandParser :: Parser Command
commandParser = subparser
  (  command "add"    (info addOptions (progDesc "Add a new SSH connection"))
  <> command "list"   (info (pure List) (progDesc "List all connections"))
  <> command "delete" (info deleteOptions (progDesc "Delete a connection"))
  <> command "edit"   (info editOptions (progDesc "Edit a connection"))
  <> command "search" (info searchOptions (progDesc "Search connections"))
  <> command "backup" (info backupOptions (progDesc "Backup operations"))
  )
  <|> flag' Version
      ( long "version"
     <> short 'v'
     <> help "Show version information"
      )

backupOptions :: Parser Command
backupOptions = subparser
  (  command "clean"  (info (pure BackupClean) (progDesc "Remove all backup files"))
  <> command "restore" (info backupRestoreOptions (progDesc "Restore a specific backup or the latest if not specified"))
  <> command "list"   (info (pure BackupList) (progDesc "List all available backups"))
  <> command "create" (info (pure BackupCreate) (progDesc "Create a new backup"))
  )
  <|> pure BackupHelp

addOptions :: Parser Command
addOptions =
  Add <$> argument str (metavar "NAME" <> help "Connection name")
      <*> argument str (metavar "USER@HOST[:PORT]" <> help "User, host, and optional port")
      <*> optional (strOption
          ( long "id-file"
         <> metavar "PATH"
         <> help "Path to the identity file"
          ))

deleteOptions :: Parser Command
deleteOptions =
  Delete <$> argument str (metavar "NAME" <> help "Connection name to delete")

editOptions :: Parser Command
editOptions =
  Edit
    <$> argument str (metavar "NAME" <> help "Connection name to edit")
    <*> argument str (metavar "USER@HOST" <> help "New user@host for the connection")
    <*> optional (strOption
          ( long "id-file"
         <> metavar "PATH"
         <> help "Path to the identity file"
          ))

searchOptions :: Parser Command
searchOptions = Search <$> argument str (metavar "QUERY" <> help "Search term")

main :: IO ()
main = execParser opts >>= runCommand
 where
  opts = info
    (commandParser <**> helper)
    (fullDesc <> progDesc "shorm - SSH connection manager" <> header
      "shorm - A simple SSH connection manager"
    )

runCommand :: Command -> IO ()
runCommand cmd = case cmd of
    Add name userHostPort idFile -> Commands.add name userHostPort idFile
    List                         -> Commands.list
    Delete name                  -> Commands.delete name
    Edit name userHost idFile    -> Commands.edit name userHost idFile
    Search query          -> Commands.search query
    Version               -> Commands.version
    BackupClean           -> Commands.getSSHConfigPath >>= Commands.backupClean
    BackupRestore maybeBackupName -> Commands.getSSHConfigPath >>= \path -> Commands.backupRestore path maybeBackupName
    BackupList            -> Commands.getSSHConfigPath >>= Commands.backupList
    BackupCreate          -> Commands.getSSHConfigPath >>= Commands.backupCreate
    BackupHelp            -> putStrLn backupHelpText

backupHelpText :: String
backupHelpText = unlines
  [ "Usage: shorm backup <subcommand>"
  , ""
  , "Available subcommands:"
  , "  clean   Remove all backup files"
  , "  restore Restore a specific backup or the latest if not specified"
  , "  list    List all available backups"
  , "  create  Create a new backup"
  ]
backupRestoreOptions :: Parser Command
backupRestoreOptions = BackupRestore <$> optional (argument str (metavar "BACKUP_NAME" <> help "Name of the backup to restore"))
