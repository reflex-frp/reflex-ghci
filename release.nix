{ reflex-platform ? import ./reflex-platform
}:
let
  rp = reflex-platform {};
  pkgs = rp.nixpkgs;
  supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
  inherit (pkgs) lib;
  haskellLib = pkgs.haskell.lib;
  commonOverrides = self: super: {
    vty = self.callHackageDirect {
      pkg = "vty";
      ver = "6.1";
      sha256 = "2cefcb5764f6b662440ba9e56c30282da37071b599a7def7fc8e5679f2602bf8";
    } {};
    vty-crossplatform = self.callHackageDirect {
      pkg = "vty-crossplatform";
      ver = "0.4.0.0";
      sha256 = "sha256-e3LnG3ouGpR/glpEof3qLj4UeURBDEKGOQqkEkpWNYs=";
    } {};
    vty-unix = self.callHackageDirect {
      pkg = "vty-unix";
      ver = "0.2.0.0";
      sha256 = "sha256-sDoxXxqo9w5eOqs2uI7S5JzWRsVrHjTBldrhPJKcqSY=";
    } {};
    reflex = self.callCabal2nix "reflex" (rp.hackGet ./dep/reflex) {};
    reflex-process = self.callCabal2nix "reflex-process" (rp.hackGet ./dep/reflex-process) {};
    reflex-vty = self.callHackageDirect {
      pkg = "reflex-vty";
      ver = "0.5.2.0";
      sha256 = "sha256-AsqIRmJiMYsnx2jBLaSvQUNOz92YiDlVu6KfR1PBCZk=";
    } {};
    reflex-fsnotify = self.callHackageDirect {
      pkg = "reflex-fsnotify";
      ver = "0.3.0.0";
      sha256 = "11wnvjk2kznfcj7m2fnfmh6xskggy0i913rw1kh64lzny7yghh39";
    } {};
    fsnotify = haskellLib.dontCheck (self.callHackage "fsnotify" "0.4.1.0" {});
  };
  ghcs = lib.genAttrs supportedSystems (system: let
    rp = reflex-platform { inherit system; __useNewerCompiler = true; };
    rpOld = reflex-platform { inherit system; __useNewerCompiler = false; };
    rpGhc = rp.ghc.override {
      overrides = commonOverrides;
    };
    rpGhc865 = rpOld.ghc.override {
      overrides = commonOverrides;
    };

    nixGhc945 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc945.override {
      overrides = self: super: commonOverrides self super // {
        hlint = self.callHackageDirect {
          pkg = "hlint";
          ver = "3.5";
          sha256 = "1np43k54918v54saqqgnd82ccd6225njwxpg2031asi70jam80x9";
        } {};

        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = pkgs.haskell.lib.dontCheck super.string-qq;
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};
      };
    };
    nixGhc961 = (import ./nixpkgs { inherit system; }).haskell.packages.ghc961.override {
      overrides = self: super: commonOverrides self super // {
        patch = self.callHackageDirect {
          pkg = "patch";
          ver = "0.0.8.2";
          sha256 = "160zqqhjg48fr3a33gffd82qm3728c8hwf8sn37pbpv82fw71rzg";
        } {};

        these-lens = self.callHackageDirect {
          pkg = "these-lens";
          ver = "1.0.1.3";
          sha256 = "0n1vkr57jz5yvy4jm15v5cs42rp342ni0gisib7aqyhibpicqs5c";
        } {};
        these = self.callHackageDirect {
          pkg = "these";
          ver = "1.2";
          sha256 = "1iaaq1fsvg8c3l0czcicshkmbbr00hnwkdamjbkljsa1qvlilaf0";
        } {};
        lens = self.callHackageDirect {
          pkg = "lens";
          ver = "5.2.2";
          sha256 = "0c4a421sxfjm1cj3nvgwkr4glll23mqnsvs2iv5qh85931h2f3cy";
        } {};

        assoc = self.callHackageDirect {
          pkg = "assoc";
          ver = "1.1";
          sha256 = "1krvcafrbj98z5hv55gq4zb1in5yd71nmz9zdiqgnywjzbrvpf75";
        } {};
        strict = self.callHackageDirect {
          pkg = "strict";
          ver = "0.5";
          sha256 = "02iyvrr7nd7fnivz78lzdchy8zw1cghqj1qx2yzbbb9869h1mny7";
        } {};
        # Jailbroken until https://github.com/audreyt/string-qq/pull/3
        string-qq = haskellLib.dontCheck super.string-qq;
        # Tests aren't compatible with transformers-0.6
        bimap = haskellLib.dontCheck super.bimap;
        exception-transformers = haskellLib.doJailbreak (haskellLib.dontCheck super.exception-transformers);

      };
    };
  in
  {
    recurseForDerivations = true;
    ghc865 = rpGhc865.callCabal2nix "reflex-ghci" (import ./src.nix) {};
    ghc810 = rpGhc.callCabal2nix "reflex-ghci" (import ./src.nix) {};
    ghc945 = nixGhc945.callCabal2nix "reflex-ghci" (import ./src.nix) {};
    ghc961 = nixGhc961.callCabal2nix "reflex-ghci" (import ./src.nix) {};
  });
  in
    ghcs
