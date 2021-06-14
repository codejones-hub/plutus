# NOTE: This file is only for transitioning services to the `bitte` infrastructure
# by providing scripts for all relevant services replacing the existing NixOS modules
# and services.

## ISSUES:
# - marlowe-playground: `POST /runghc` on AWS this is hacked to forward to web-ghc insteaed,
#   but now we don't have this magic anymore and it needs to be proxied to webghc url instead.
#   For now the "solution" is to just make ghc with all required packages available to the
#   marlowe-playground but of course this way the dedicated web-ghc-service isn't even used.
# - clients: The clients are currently served via https://www.npmjs.com/package/http-server

## OVERVIEW:
#
# * web-ghc: 
#            - ghc behind a REST endpoint
#            - resource-hungry in cpu and memory
#            - should probably be rate-limited/protected against DoS
#            - defnitely needs multiple instances when user base grows
#
# * plutus-playground-server: 
#            - backend of the plutus playground
#            - simple REST service that talks to the client and makes requests to web-ghc
#            - talks to github for saving files as github gists
#
# * plutus-playground-client:
#            - purescript plutus playground frontend
#            - talks to plutus-playground-server
#            - currently served via https://www.npmjs.com/package/http-server
#
# * plutus-playground-server: 
#            - backend of the marlowe playground
#            - invokes ghc directly (see note under ISSUES above)
#
# * marlowe-playground-client:
#            - purescript marlowe playground frontend
#            - talks to marlowe-playground-server
#            - currently served via https://www.npmjs.com/package/http-server
#
# * marlowe-web:
#            - simple static website
#            - currently served via https://www.npmjs.com/package/http-server
#
# * pab:
#            - provides several endpoints
#            - currently only used by marlowe-dashboard


{ topLevel ? import ./. { } }:
let
  inherit (topLevel) pkgs plutus plutus-pab plutus-playground marlowe-playground marlowe-app marlowe-web marlowe-companion-app marlowe-follow-app;
  inherit (pkgs) writeShellScriptBin writeText lib;

  # run-web-ghc: Start the web-ghc service
  # - DEPENDS-ON: --
  run-web-ghc =
    { port ? 8009         # port to bind to
    , ipAddr ? "0.0.0.0"  # ip address to bind to
    , ghcWithPlutus ? plutus.haskell.project.ghcWithPackages (ps: [
        ps.plutus-core
        ps.plutus-tx
        ps.plutus-contract
        ps.plutus-ledger
        ps.playground-common
      ])
    }: writeShellScriptBin "run-web-ghc" ''
      PATH=${ghcWithPlutus}/bin:$PATH
      ${plutus.haskell.project.hsPkgs.web-ghc.components.exes.web-ghc-server}/bin/web-ghc-server webserver -b ${ipAddr} -p ${builtins.toString port}
    '';

  # run-plutus-playground: Start the plutus-playground-server
  # DEPENDS-ON:
  # - Service: run-web-ghc
  # - ENV: JWT_SIGNATURE (secret)
  # - ENV: GITHUB_CLIENT_SECRET (secret)
  # - ENV: GITHUB_CLIENT_ID
  # - ENV: WEBGHC_URL
  # - ENV: FRONTEND_URL
  # - ENV: GITHUB_CALLBACK_PATH
  run-plutus-playground-server =
    { port ? 4003         # port to bind to
    }: writeShellScriptBin "run-plutus-playground-server" ''
      ${plutus-playground.server}/bin/plutus-playground-server webserver -p ${builtins.toString port}
    '';

  # run-plutus-playground-client: Serve the marlowe-playground client
  # DEPENDS-ON: -
  run-plutus-playground-client =
    { port ? 8081
    , serverPort ? 4003
    }: writeShellScriptBin "run-plutus-playground-client" ''
      ${pkgs.nodePackages.http-server}/bin/http-server ${plutus-playground.client}\
        --brotli\
        -P http://localhost:${builtins.toString serverPort} \
        -p ${builtins.toString port}
    '';

  # run-marlowe-playground: Start the marlowe-playground-server
  # DEPENDS-ON:
  # - Service: run-web-ghc
  # - ENV: JWT_SIGNATURE (secret)
  # - ENV: GITHUB_CLIENT_SECRET (secret)
  # - ENV: GITHUB_CLIENT_ID
  # - ENV: FRONTEND_URL
  # - ENV: GITHUB_CALLBACK_PATH
  run-marlowe-playground-server =
    { port ? 4004
    , ghcWithMarlowe ? plutus.haskell.project.ghcWithPackages (ps: [ ps.marlowe ])
    , killallz3 ? pkgs.writeShellScriptBin "killallz3" ''
        kill -9 $(ps aux | grep z3 | grep -v grep | awk '{print $2}')
      ''
    }: writeShellScriptBin "run-marlowe-playground-server" ''
      PATH=${ghcWithMarlowe}/bin:${killallz3}/bin:$PATH
      ghc-pkg list |grep plutus
      ${marlowe-playground.server}/bin/marlowe-playground-server webserver -p ${builtins.toString port}
    '';

  # run-marlowe-playground-client: Serve the marlowe-playground client
  # DEPENDS-ON: -
  run-marlowe-playground-client =
    { port ? 8080
    , serverPort ? 4004
    }: writeShellScriptBin "run-marlowe-playground-client" ''
      ${pkgs.nodePackages.http-server}/bin/http-server ${marlowe-playground.client}\
        --brotli\
        -P http://localhost:${builtins.toString serverPort} \
        -p ${builtins.toString port}
    '';

  # run-marlowe-playground-client: Serve the marlowe-web static website
  # DEPENDS-ON: -
  run-marlowe-web =
    { port ? 8082
    }: writeShellScriptBin "run-marlowe-web" ''
      ${pkgs.nodePackages.http-server}/bin/http-server ${marlowe-web}\
        --brotli\
        -p ${builtins.toString port}
    '';


  # run-pab: Start the pab service
  # DEPENDS-ON:
  # - config file
  run-plutus-pab =
    let
      plutus-exe = plutus-pab.pab-exes.plutus-pab;
      pabConfig = {
        dbConfig = {
          dbConfigFile = "/tmp/pab-core.db";
          dbConfigPoolSize = 20;
        };

        pabWebserverConfig = {
          baseUrl = "http://localhost:8000";
          staticDir = ".";
        };

        walletServerConfig = {
          baseUrl = "http://localhost:8001";
          wallet = {
            getWallet = 1;
          };
        };

        nodeServerConfig = {
          mscBaseUrl = "http://localhost:8002";
          mscSocketPath = "/tmp/node-server.sock";
          mscRandomTxInterval = 20000000;
          mscSlotConfig = {
            scZeroSlotTime = "2020-07-29T21:44:51Z";
            scSlotLength = 1;
          };
          mscKeptBlocks = 100000;
          mscBlockReaper = {
            brcInterval = 6000000;
            brcBlocksToKeep = 100000;
          };
          mscInitialTxWallets = [
            { getWallet = 1; }
            { getWallet = 2; }
            { getWallet = 3; }
          ];
        };

        chainIndexConfig = {
          ciBaseUrl = "http://localhost:8003";
          ciWatchedAddresses = [ ];
        };

        requestProcessingConfig = {
          requestProcessingInterval = 1;
        };

        signingProcessConfig = {
          spBaseUrl = "http://localhost:8004";
          spWallet = {
            getWallet = "1";
          };
        };

        metadataServerConfig = {
          mdBaseUrl = "http://localhost:8005";
        };
      };
    in
    { configFile ? writeText "pab.yaml" (builtins.toJSON pabConfig)
    , contracts ? [
        "${marlowe-app}/bin/marlowe-app"
        "${marlowe-companion-app}/bin/marlowe-companion-app"
        "${marlowe-follow-app}/bin/marlowe-follow-app"
      ]
    }: writeShellScriptBin "run-pab" ''
      echo "[run-pab]: creating new database"
      rm -rf ${pabConfig.dbConfig.dbConfigFile}
      ${plutus-exe}/bin/plutus-pab migrate ${pabConfig.dbConfig.dbConfigFile}

      echo "[run-pab]: installing contracts"
      ${lib.concatMapStringsSep "\n" (p: "${plutus-exe}/bin/plutus-pab --config=${configFile} contracts install --path ${p}") contracts}

      echo "[run-pab]: Starting PAB"
      ${plutus-exe}/bin/plutus-pab --config=${configFile} all-servers
    '';

in
{
  shell = pkgs.mkShell {
    buildInputs = [
      (run-web-ghc { })
      (run-plutus-pab { })
      (run-marlowe-playground-server { })
      (run-marlowe-playground-client { })
      (run-marlowe-web { })
      (run-plutus-playground-server { })
      (run-plutus-playground-client { })
    ];

  };
  inherit run-web-ghc run-plutus-pab;
  inherit run-marlowe-playground-server run-marlowe-playground-client run-marlowe-web;
  inherit run-plutus-playground-server run-plutus-playground-client;
}
