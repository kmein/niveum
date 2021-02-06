{ config, pkgs, lib, ... }:
let
  inherit (import <niveum/lib>) kieran;
in {
  imports = [
    ./hardware-configuration.nix
    # <niveum/configs/hass>
    <niveum/configs/distrobump.nix>
    <niveum/configs/spacetime.nix>
    <niveum/configs/sshd.nix>
    <niveum/configs/moodle-dl.nix>
    <niveum/configs/save-space.nix>
    <niveum/configs/wifi.nix>
    <niveum/configs/tmux.nix>
    <niveum/configs/version.nix>
    <niveum/configs/traadfri.nix>
    <niveum/modules/retiolum.nix>
    {
      services.weechat.enable = true;
      users.extraUsers.weechat = {
        useDefaultShell = true;
        openssh.authorizedKeys.keys = kieran.sshKeys pkgs;
      };
    }
  ];

  nix.nixPath = [ "/var/src" ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "toum";

  environment.variables.TERM = "linux";

  environment.systemPackages = with pkgs; [
    git vim htop wget reptyr
    raspberrypi-tools
  ];

  users.mutableUsers = false;

  networking.retiolum = {
    ipv4 = "10.243.2.3";
    ipv6 = "42:0:3c46:56af:d12b:affd:8390:df22";
  };

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = builtins.readFile <system-secrets/retiolum.key>;
    mode = "400";
  };

  system.stateVersion = "20.03";
}
