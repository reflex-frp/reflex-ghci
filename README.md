reflex-ghci
==============

[![hackage](https://img.shields.io/hackage/v/reflex-ghci.svg)](https://hackage.haskell.org/package/reflex-ghci) [![hackage-ci](https://matrix.hackage.haskell.org/api/v2/packages/reflex-ghci/badge)](https://matrix.hackage.haskell.org/#/package/reflex-ghci) [![travis-ci](https://api.travis-ci.org/reflex-frp/reflex-ghci.svg?branch=develop)](https://travis-ci.org/reflex-frp/reflex-ghci)

A functional-reactive wrapper around GHCi that uses filesystem notifications to automatically reload haskell source files.

![screenshot](screenshot.png)

This package includes a [reflex-vty](https://github.com/reflex-frp/reflex-vty)-based executable, shown above. Module information (errors, warnings, etc) is shown in a scrollable pane on the top half of the screen and the output of any expression you (optionally) choose to evaluate is shown in a scrollable pane on the bottom half. The panes are resizable using the mouse.

Inspired by the fantastic [ghcid](https://github.com/ndmitchell/ghcid) project.
