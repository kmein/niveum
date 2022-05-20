{
  lib,
  config,
  pkgs,
  ...
}: let
  inherit (import <niveum/lib>) kieran retiolumAddresses;
in {
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/monitoring.nix>
    <niveum/configs/nix.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/modules/retiolum.nix>
  ];

  nix.nixPath = ["/var/src"];

  networking = {
    firewall.allowedTCPPorts = [80 443];
    hostName = "ful";
    interfaces.enp0s3.useDHCP = true;
    retiolum = retiolumAddresses.ful;
    useDHCP = false;
  };

  system.stateVersion = "21.11";

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;
    sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  security.acme = {
    acceptTerms = true;
    email = kieran.email;
  };

  environment.systemPackages = [pkgs.vim pkgs.git pkgs.tmux pkgs.python3];
}