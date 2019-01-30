{ config, pkgs, ... }:
{
  # for kdeconnect
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  home-manager.users.kfm = {
    services.kdeconnect = {
      enable = true;
      indicator = true;
    };
  };
}
