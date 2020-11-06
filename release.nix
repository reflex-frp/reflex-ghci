{ reflex-platform-fun ? import ./reflex-platform
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]
}:
let
  haskellOverlay = self: super: {
  };

  self-reflex-platform = reflex-platform-fun {};

  perPlatform = self-reflex-platform.nixpkgs.lib.genAttrs supportedSystems (system:
    let
      reflex-platform = reflex-platform-fun {
        inherit system;
        haskellOverlaysPost = [haskellOverlay];
      };
      packages = {
        reflex-ghci = reflex-platform.ghc.callPackage ./default.nix {};
      };
    in packages // {
      cache = reflex-platform.pinBuildInputs
        "reflex-ghci-${system}"
        (builtins.attrValues packages);
    }
  );

  metaCache = self-reflex-platform.pinBuildInputs
    "reflex-ghci-everywhere"
    (map (a: a.cache) (builtins.attrValues perPlatform));
in
  perPlatform // { inherit metaCache; }
