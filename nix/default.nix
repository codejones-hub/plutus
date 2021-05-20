{ system ? builtins.currentSystem
, crossSystem ? null
, config ? { }
, overlays ? [ ]
, useFlakes ? false
, sourcesOverride ? { }
, checkMaterialization ? false
, enableHaskellProfiling ? false
}:
let
  sources = import ./sources.nix { inherit system pkgs; } // sourcesOverride;
  iohkNix = import sources.iohk-nix { };
  haskellNix = import sources."haskell.nix" {
    inherit system;
    sourcesOverride = {
      hackage = sources."hackage.nix";
      stackage = sources."stackage.nix";
    };
  };

  # For haskell.nix a different overlay has to be used for
  # normal evaluation or when evaluated in a flake build
  haskellNixOverlays = if useFlakes then [ haskellNix.allOverlays.combined-eval-on-build ] else haskellNix.overlays;

  extraOverlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNixOverlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix
    # our own overlays:
    ++ [
      # Modifications to derivations from nixpkgs
      (import ./overlays/nixpkgs-overrides.nix)
      # fix r-modules
      (import ./overlays/r.nix)
    ];

  pkgs = import sources.nixpkgs {
    inherit system crossSystem;
    overlays = extraOverlays ++ overlays;
    config = haskellNix.config // config;
  };

  plutus = import ./pkgs { inherit pkgs checkMaterialization enableHaskellProfiling sources; };

in
{
  inherit pkgs plutus sources;
}
