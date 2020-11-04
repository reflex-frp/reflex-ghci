{ callCabal2nix, ... }:
let
  src = builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
    "release.nix"
    ".git"
    "dist"
    "dist-newstyle"
    "cabal.project"
    ".travis.yml"
  ])) ./.;
in callCabal2nix "reflex-ghci" src {}
