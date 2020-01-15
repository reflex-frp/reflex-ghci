{ p ? import ./reflex-platform {}
}:
let
  inherit (p.nixpkgs) lib;
in p.ghc.callCabal2nix "reflex-ghci" ./. {}
