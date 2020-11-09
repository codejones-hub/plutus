{ pkgs, set-git-rev, haskell, docs, buildPlayground }:

let

  runtimeGhc = haskell.packages.ghcWithPackages (ps: [ ps.marlowe ps.marlowe-playground-server ]);

  playground-exe = set-git-rev haskell.packages.marlowe-playground-server.components.exes.marlowe-playground-server;

  playground-exe-name = "marlowe-playground-server";

  client = buildPlayground {
    srcDir = ./.;
    name = "marlowe-playground-client";
    pscPackages = pkgs.callPackage ./packages.nix { };
    spagoPackages = pkgs.callPackage ./spago-packages.nix { };
    inherit runtimeGhc;
    inherit playground-exe playground-exe-name;
  };
in
{
  inherit client;
  tutorial = docs.marlowe-tutorial;
}
