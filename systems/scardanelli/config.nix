{ config, pkgs, ... }:
{
  imports =
    [ ../../main.nix
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "scardanelli";

  system.stateVersion = "19.03";
}
