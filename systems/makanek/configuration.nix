{ lib, config, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran retiolumAddresses;
in
{
  imports = [
    ./gitea.nix
    ./hardware-configuration.nix
    ./hedgedoc.nix
    ./matterbridge.nix
    ./menstruation.nix
    ./monitoring
    ./moodle-dl-borsfaye.nix
    ./names.nix
    ./nextcloud.nix
    ./radio
    ./retiolum-map.nix
    ./tarot.nix
    ./urlwatch.nix
    ./weechat.nix
    <niveum/configs/monitoring.nix>
    <niveum/configs/nix.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/telegram-bots>
    <niveum/modules/retiolum.nix>
  ];

  networking = {
    firewall.allowedTCPPorts = [ 80 443 ];
    hostName = "makanek";
    interfaces.ens3.useDHCP = true;
    retiolum = retiolumAddresses.makanek;
    useDHCP = false;
  };

  system.stateVersion = "20.03";

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

  environment.systemPackages = [ pkgs.vim pkgs.git pkgs.tmux pkgs.python3 ];
}
