{
  description = "Flake for Plutus";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils?rev=b543720b25df6ffdfcf9227afafc5b8c1fabfae8";
  };

  outputs = { self, flake-utils, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        topLevel = import ./. {
          inherit system;
          useFlakes = true;
        };

        inherit (topLevel) pkgs plutus ownOverlays;
        inherit (plutus) haskell iohkNix;
        inherit (plutus.lib) buildPursPackage buildNodeModules filterNpm gitignore-nix;
      in
      rec {
        packages = rec {
          marlowe-playground-client = topLevel.marlowe-playground.client;
          marlowe-playground-server = topLevel.marlowe-playground.server;
          plutus-playground-client = topLevel.plutus-playground.client;
          plutus-playground-server = topLevel.plutus-playground.server;
          marlowe-website = topLevel.marlowe-web;
          web-ghc-server = plutus.haskell.project.hsPkgs.web-ghc.components.exes.web-ghc-server;
        };
      }));
}
