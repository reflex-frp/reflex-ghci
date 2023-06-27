{ pkgs ?  (import ./nixpkgs {})
}:
pkgs.mkShell {
  name = "reflex-ghci";
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
  inputsFrom = [
    (import ./release.nix {}).${builtins.currentSystem}.ghc810.env
  ];
}
