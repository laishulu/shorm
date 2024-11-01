# shorm - SSH Connection Manager

`shorm` is a simple tool to manage SSH connections by editing ~/.ssh/config
directly. It's a Haskell implementation of StormSSH, as the original project has
been discontinued.

## Installation

To install shorm using Homebrew or Linuxbrew, run the following commands:

```bash
brew tap laishulu/shorm https://github.com/laishulu/shorm
brew install shorm
```

You can also download the pre-built binaries directly from
[here](https://github.com/laishulu/shorm/releases).

## Usage

```bash
# Add a new connection
shorm add <name> <[user@]host[:port]> [--id-file <path>]

# List all connections
shorm list

# Search connections
shorm search <query>

# Edit a connection
shorm edit <name> <[new_user@]new_host[:new_port]> [--id-file <path>]

# Delete a connection
shorm delete <name>

# Backup operations
shorm backup <subcommand>

# Show version information
shorm --version
```

## Backup Subcommands

```bash
# Create a new backup
shorm backup create

# List all available backups
shorm backup list

# Restore a specific backup or the latest if not specified
shorm backup restore [BACKUP_NAME]

# Remove all backup files
shorm backup clean
```

## Examples

```bash
# Add a new connection with user, host, and port
shorm add myserver user@example.com:2222

# Add a connection with user, host, and identity file
shorm add homeserver alice@home.example.com --id-file ~/.ssh/home_rsa

# Add a connection with only host (no user specified, default port 22)
shorm add workserver work.example.com

# Add a connection with host and port (no user specified)
shorm add dbserver database.example.com:5432

# List all connections
shorm list

# Search for connections containing "server"
shorm search server

# Edit an existing connection (changing user, host, and port)
shorm edit myserver newuser@newexample.com:2223

# Edit an existing connection (changing only the host and identity file)
shorm edit workserver newwork.example.com --id-file ~/.ssh/work_rsa

# Delete a connection
shorm delete homeserver

# Create a new backup
shorm backup create

# List all available backups
shorm backup list

# Restore the latest backup
shorm backup restore

# Restore a specific backup
shorm backup restore config-20231029-123456

# Remove all backup files
shorm backup clean

# Show version information
shorm --version
```

Notes:
- If the port is not specified, it defaults to 22.
- If the user is not specified, the system will use the default SSH behavior (usually your current username).
- When editing, you can change any combination of user, host, port, and identity file. Omitted parts will remain unchanged.
- The `--id-file` option allows you to specify a custom identity file (private key) for the connection.
