with import <nixpkgs> {};

((pkgs.callPackage <code/Haskell/reflex-platform> { enableLibraryProfiling = true;
}).ghcjs.callCabal2nix "grindcraft" ./. {
  reflex-dom-contrib = null;
}).env
