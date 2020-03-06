{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc865" }:

let
  inherit (nixpkgs) pkgs;
  f = import ./resource-id.nix;
  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};
in
  drv
