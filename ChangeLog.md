# Revision history for reflex-ghci

## 0.1.3.0

* Library: Fix parsing of "Failed, n modules loaded." message
* Library: Properly initialize when GHCi version message is the first output line
* Executable: Terminate ghci process when ctrl-c is received
* Library: Don't use a separate process group. Send termination signal to process, not group
* Library: Use `watchDirectoryTree` from `Reflex.FSNotify` to avoid symlink cycles when enumerating directories

## 0.1.2.0

* Extract console GHCi widgets from the executable into a library module
* Executable: Run with -Wall when repl command is unspecified

## 0.1.1.0

* Executable: Fix option parser so that there is no expression to evaluate by default

## 0.1.0.0

* Initial release. A reflex-process wrapper for GHCi and cabal repl commands and a reflex-vty-based executable.
