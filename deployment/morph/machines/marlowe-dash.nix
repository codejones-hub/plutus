{ pkgs, config, lib, tfinfo, ... }:
let
  marlowe-app = pkgs.hello;
  marlowe-companion-app = pkgs.hello;
  plutus-pab = pkgs.hello;
  marlowe-dashboard = pkgs.hello;
in
{
  imports = [
    ./std.nix
    ../../../nix/modules/pab.nix
  ];

  networking = {
    firewall.allowedTCPPorts = [ 22 80 9080 ];
  };

  services.pab = {
    enable = true;
    pab-package = plutus-pab;
    contracts = [ "${marlowe-app}/bin/marlowe-app" "${marlowe-companion-app}/bin/marlowe-companion-app" ];
    staticContent = marlowe-dashboard;
    dbFile = "/var/lib/pab/pab-core.db";
    defaultWallet = 1;
    webserverPort = 9080;
    walletPort = 8081;
    nodePort = 8082;
    chainIndexPort = 8083;
    signingProcessPort = 8084;
    metadataPort = 8085;
  };

}
