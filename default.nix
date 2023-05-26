{ callCabal2nix, ... }:
let src = import ./src.nix;
in callCabal2nix "reflex-ghci" src {}
