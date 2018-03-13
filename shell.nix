{ compiler ? "ghc8_2_1" }:

with import <nixpkgs> {};

((pkgs.callPackage <code/Haskell/reflex-platform> { enableLibraryProfiling = true;
}).${compiler}.callCabal2nix "grindcraft" ./. {
  reflex-dom-contrib = null;
}).env
