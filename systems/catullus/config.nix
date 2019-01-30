{ config, pkgs, ... }:
{
  imports = [ ../slim.nix ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";
}
