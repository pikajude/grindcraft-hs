with import <nixpkgs> {};

((pkgs.callPackage <code/Haskell/reflex-platform> { enableLibraryProfiling = true;
}).ghc8_2_1.callCabal2nix "grindcraft" ./. {
  reflex-dom-contrib = null;
}).env
