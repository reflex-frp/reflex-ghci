{ pkgs ?  (import ./nixpkgs {}
}
pkgs.mkShell {
  name = "reflex-ghci";
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
  inputsFrom = [
    (import ./release.nix {}).ghc810.env
  ];
}
