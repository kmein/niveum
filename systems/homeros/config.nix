{ config, pkgs, ... }:
{
  imports =
    [ <niveum/main.nix>
    ];

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "homeros";

  system.stateVersion = "19.03";
}
