{ pkgs, set-git-rev, haskell, docs, buildPlayground }:

let

  playground-exe = set-git-rev haskell.packages.plutus-playground-server.components.exes.plutus-playground-server;

  playground-exe-name = "plutus-playground-server";

  runtimeGhc = haskell.packages.ghcWithPackages (ps: [
    ps.playground-common
    ps.plutus-playground-server
    ps.plutus-use-cases
  ]);

  client = buildPlayground {
    srcDir = ./.;
    name = "plutus-playground-client";
    pscPackages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    inherit runtimeGhc;
    inherit playground-exe playground-exe-name;
  };
in
{
  inherit client;
  tutorial = docs.site;
  haddock = docs.plutus-haddock-combined;
}
