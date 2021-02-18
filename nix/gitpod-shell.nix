let
  packages = import ../nix { rev = "in-gitpod-nix-shell"; };
  inherit (packages) pkgs plutus plutusMusl;
  inherit (pkgs) stdenv lib utillinux python3 nixpkgs-fmt;
  inherit (plutus) haskell agdaPackages stylish-haskell sphinxcontrib-haddock nix-pre-commit-hooks;
  inherit (plutus) agdaWithStdlib;
  inherit (plutus) purty purty-pre-commit purs spargo;

  # For Sphinx, and ad-hoc usage
  sphinxTools = python3.withPackages (ps: [ sphinxcontrib-haddock.sphinxcontrib-domaintools ps.sphinx ps.sphinx_rtd_theme ]);

  # build inputs from nixpkgs ( -> ./nix/default.nix )
  nixpkgsInputs = (with pkgs; [
    # pkgs.sqlite-analyzer -- Broken on 20.03, needs a backport
    # awscli
    # cacert
    # ghcid
    niv
    # nixpkgs-fmt
    # nodejs
    # pass
    # shellcheck
    # sqlite-interactive
    # stack
    # terraform_0_12
    # yarn
    # yubikey-manager
    # z3
    zlib
  ]);
  #  ++ (lib.optionals (!stdenv.isDarwin) [ rPackages.plotly R ]));

  # local build inputs ( -> ./nix/pkgs/default.nix )
  localInputs = (with plutus; [
    cabal-install
    fixPurty
    fixStylishHaskell
    haskell-language-server
    hie-bios
    hlint
    purs
    purty
    spago
    stylish-haskell
    updateClientDeps
    updateMetadataSamples
  ]);

in [
  (haskell.project.shellFor {}).ghc
] ++ nixpkgsInputs ++ localInputs

# ++ [ agdaWithStdlib sphinxTools ]
