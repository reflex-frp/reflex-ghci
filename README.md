reflex-ghci
==============

[![hackage](https://img.shields.io/hackage/v/reflex-ghci.svg)](https://hackage.haskell.org/package/reflex-ghci) [![hackage-ci](https://matrix.hackage.haskell.org/api/v2/packages/reflex-ghci/badge)](https://matrix.hackage.haskell.org/#/package/reflex-ghci) [![travis-ci](https://api.travis-ci.org/reflex-frp/reflex-ghci.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-ghci)

![screenshot](screenshot.png)

Library
-------
A functional-reactive wrapper around GHCi that uses filesystem notifications to automatically reload haskell source files.

`Reflex.Process.GHCi` provides the core GHCi process-running infrastructure. If you want to run your own GHCi, directly control when it reloads, or build your own custom interface, look there.

`Reflex.Vty.GHCi` provides a few widgets that are useful when building a console GHCi interface. Use these components to assemble your own vty GHCi runner.

Executable
----------

This package includes a [reflex-vty](https://github.com/reflex-frp/reflex-vty)-based executable, shown above. Module information (errors, warnings, etc) is shown in a scrollable pane on the top half of the screen and the output of any expression you (optionally) choose to evaluate is shown in a scrollable pane on the bottom half. The panes are resizable using the mouse.

```bash
$ reflex-ghci -h
Welcome to reflex-ghci 0.1.4.0

Usage: reflex-ghci [-c|--command COMMAND] [-e|--expression EXPR]
  Run a Haskell REPL that automatically reloads when source files change.

Available options:
  -c,--command COMMAND     The ghci/cabal repl command to
                           run (default: "cabal repl --repl-options=-Wall")
  -e,--expression EXPR     The optional expression to evaluate once modules have
                           successfully loaded
  -h,--help                Show this help text

```

Acknowledgements
----------------
Inspired by the fantastic [ghcid](https://github.com/ndmitchell/ghcid) project.
