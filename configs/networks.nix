{
  networking.hosts = {
    "192.168.178.27" = [ "printer.local" ];
  };

  networking.wireless.enable = true;
  networking.wireless.networks = {
    Aether = { pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a"; };
    "Asoziales Netzwerk" = { pskRaw = "8e234041ec5f0cd1b6a14e9adeee9840ed51b2f18856a52137485523e46b0cb6"; };
    "c-base-public" = {};
  };
}
