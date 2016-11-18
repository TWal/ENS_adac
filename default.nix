{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let
  inherit (nixpkgs) pkgs haskellPackages;
  hpkgs = pkgs.haskell.packages.${compiler};
in hpkgs.mkDerivation {
  pname = "adac";
  version = "0.5";
  src = ./.;
  buildDepends = with hpkgs; [ array ];
  buildTools = with hpkgs; [ happy alex ghc ];
  enableSplitObjs = false;
  license = "WTFPL";
}

