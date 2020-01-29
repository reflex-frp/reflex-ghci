{ mkDerivation, base, bytestring, dependent-sum, directory
, filepath, fsnotify, mtl, optparse-applicative, primitive, process
, ref-tf, reflex, reflex-fsnotify, reflex-process, reflex-vty
, regex-tdfa, stdenv, temporary, text, unix, vty, which
, ghc
}:
mkDerivation {
  pname = "reflex-ghci";
  version = "0.1.3.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring directory filepath fsnotify process reflex
    reflex-fsnotify reflex-process reflex-vty regex-tdfa text unix vty
  ];
  executableHaskellDepends = [
    base optparse-applicative process reflex reflex-process reflex-vty
    text vty
  ];
  testHaskellDepends = [
    base bytestring dependent-sum directory mtl primitive process
    ref-tf reflex reflex-process reflex-vty temporary text which
  ];
  testToolDepends = [
    ghc
  ];
  description = "A GHCi widget library for use in reflex applications";
  license = stdenv.lib.licenses.bsd3;
}
