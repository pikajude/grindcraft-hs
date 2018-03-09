with import <nixpkgs> {};

((pkgs.callPackage <code/Haskell/reflex-platform> {}).ghc8_2_1.callCabal2nix "grindcraft" ./. {
  reflex-dom-contrib = null;
}).env
