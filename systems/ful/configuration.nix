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
    <retiolum/modules/retiolum>
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
    defaults.email = kieran.email;
  };

  users.users.root.passwordFile = toString <system-secrets/root.password>;

  environment.systemPackages = [pkgs.vim pkgs.git pkgs.tmux pkgs.python3];

  # since 22.05 timeout fails?
  systemd.services.systemd-networkd-wait-online.enable = false;
}
