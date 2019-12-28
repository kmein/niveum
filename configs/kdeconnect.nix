{
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
  };

  home-manager.users.me = {
    services.kdeconnect = {
      enable = false;
      indicator = false;
    };
  };
}
