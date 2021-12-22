{ lib, config, pkgs, ... }:
let
  inherit (import <niveum/lib>) kieran retiolumAddresses;
in
{
  imports = [
    ./hardware-configuration.nix
    <niveum/configs/hedgedoc.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/nextcloud.nix>
    <niveum/configs/moodle-dl/borsfaye.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/monitoring/pull.nix>
    <niveum/configs/monitoring/push.nix>
    <niveum/configs/version.nix>
    <niveum/configs/radio>
    <niveum/configs/gitea.nix>
    <niveum/configs/retiolum-map.nix>
    <niveum/configs/names.nix>
    <niveum/configs/menstruation.nix>
    <niveum/configs/telegram-bots>
    <niveum/configs/weechat.nix>
    <niveum/configs/urlwatch.nix>
    <niveum/configs/matterbridge.nix>
    <niveum/configs/tarot.nix>
    <niveum/modules/retiolum.nix>
  ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  nixpkgs.config = {
    allowUnfree = true;
    packageOverrides = pkgs: {
      writeDashBin = pkgs.writers.writeDashBin;
      writeDash = pkgs.writers.writeDash;
    };
  };

  networking.useDHCP = false;
  networking.interfaces.ens3.useDHCP = true;

  networking.hostName = "makanek";

  system.stateVersion = "20.03";

  boot.loader.grub.devices = [ "/dev/sda" ];

  services.openssh.enable = true;

  networking.retiolum = retiolumAddresses.makanek;

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <system-secrets/retiolum.key>;
    mode = "400";
  };

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedOptimisation = true;
    recommendedProxySettings = true;
    recommendedTlsSettings = true;

   # Only allow PFS-enabled ciphers with AES256
   sslCiphers = "AES256+EECDH:AES256+EDH:!aNULL";
  };

  security.acme = {
    acceptTerms = true;
    email = kieran.email;
  };

  environment.systemPackages = [ pkgs.vim pkgs.git pkgs.tmux pkgs.python3Packages.python ];
}
