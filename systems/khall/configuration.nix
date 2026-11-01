{ lib, ... }:
{
  networking.hostName = "khall";
  networking.networkmanager.enable = true;
  system.stateVersion = "25.11";

  # evaluating the flake needs more memory than the Pi 3 has; deploy instead
  system.autoUpgrade.enable = lib.mkForce false;
}
