{ pkgs, config, ... }:
{
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless = {
    enable = true;
    userControlled.enable = true;
    networks = {
      Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
      "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
      "c-base-public" = {};
    };
  };

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

  networking.retiolum = {
    scardanelli = {
      ipv4 = "10.243.2.2";
      ipv6 = "42:0:3c46:4007:5bce:f1bc:606b:2b18";
    };
    homeros = {
      ipv4 = "10.243.2.1";
      ipv6 = "42:0:3c46:53e:e63d:e62a:56ea:c705";
    };
  }.${config.networking.hostName};

  environment.etc."tinc/retiolum/rsa_key.priv" = {
    text = (import ../secrets.nix).retiolum.${config.networking.hostName}.privateKey;
    mode = "400";
  };
}
