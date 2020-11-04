# Revision history for reflex-ghci

## 0.1.5.0

* Fix various circumstances under which reflex-ghci would hang due to failure to properly parse GHCi output or failure to detect filesystem changes (the latter particularly on macOS)

## 0.1.4.2

* Tests: Ensure proper shutdown.
* Tests: Fix tests for GHC 8.8.4

## 0.1.4.1

* Library: Require at least version 0.7.1 of `reflex`.
* Library: Require at least version 0.3 of `reflex-process`.
* Library: Expand version bounds for `regex-tdfa` and `vty`.
* Tests: Include test fixtures in sdist.
* Tests: Fix some bugs.

## 0.1.4.0

* Library: Export `shutdown` and `getExitEvent` to make it easier for library users to cleanly exit
* Library: Fix regex to capture "Failed, one module loaded"
* Tests: Add basic test suite, covering module loading, expression execution, exceptions, and filesystem notification-based reloading.
* Library: Reset module output pane scroll position on reload events (like we do with the expression output pane).
* Executable: Show executable version number (taken from cabal file) in help output

## 0.1.3.1

* Update for compatibility with `reflex-process`-0.2.0.0.

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
