{
  description = "A very basic flake";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils/master";
    devshell.url = "github:numtide/devshell/master";
  };

  outputs = { self, nixpkgs, flake-utils, devshell }:

    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = final: prev: {
          haskellPackages = prev.haskellPackages.override {
            overrides = hself: hsuper: {

              resource-id = hself.callCabal2nix "resource-id"
                (final.nix-gitignore.gitignoreSourcePure [ ./.gitignore ] ./.)
                { };
            };
          };
          resource-id = final.haskell.lib.justStaticExecutables
            final.haskellPackages.resource-id;

        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ devshell.overlay overlay ];
        };

        myHaskellEnv = (pkgs.haskellPackages.ghcWithHoogle (p:
          with p;
          [ cabal-install ormolu hlint brittany ]
          ++ pkgs.resource-id.buildInputs));

      in rec {

        defaultPackage = pkgs.resource-id;
        devShell = pkgs.devshell.mkShell {
          name = "dev-shell";
          env = [
            {
              name = "HIE_HOOGLE_DATABASE";
              value = "${myHaskellEnv}/share/doc/hoogle/default.hoo";
            }
            {
              name = "NIX_GHC";
              value = "${myHaskellEnv}/bin/ghc";
            }
            {
              name = "NIX_GHCPKG";
              value = "${myHaskellEnv}/bin/ghc-pkg";
            }
          ];
          packages = [ myHaskellEnv pkgs.nixpkgs-fmt pkgs.hpack ];
        };
      });
}
