{ config, pkgs, ... }:
{
  imports = [];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  networking.hostName = "catullus";
}
