{ reflex-platform-fun ? import ./reflex-platform
}:
(reflex-platform-fun {
  haskellOverlaysPost = [
    (self: super: {
      reflex = self.callHackageDirect {
        pkg = "reflex";
        ver = "0.7.1.0";
        sha256 = "0a933xz7yl931m90bbwi9akfz77q6px36grlx6wba55mn1klpn27";
      } {};

      reflex-fsnotify = self.callHackageDirect {
        pkg = "reflex-fsnotify";
        ver = "0.2.1.1";
        sha256 = "1snbvf8z942fpb0r5spaxcdc036v6b1akgdscpfghz81bdvcxy8i";
      } {};

      reflex-process = self.callHackageDirect {
        pkg = "reflex-process";
        ver = "0.3.0.0";
        sha256 = "0xx8gzs7c60zh8rj794hyisljp0gwb26m34ns8z9xgp1k8jgkdgj";
      } {};

      reflex-vty = self.callHackageDirect {
        pkg = "reflex-vty";
        ver = "0.1.4.0";
        sha256 = "0djs7y4mmkb2q5hvp1fr1gn81k08hzab8v3c6qvh7nyn1fdh8zvh";
      } {};
    })
  ];
}).ghc.callPackage
  (builtins.filterSource (path: type: !(builtins.elem (baseNameOf path) [
      "release.nix"
      ".git"
      "dist"
      "dist-newstyle"
      "cabal.project"
      ".travis.yml"
    ])) ./.
  )
  {}
