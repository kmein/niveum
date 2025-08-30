{ config, ... }:
{
  networking.wireless = {
    enable = true;
    secretsFile = config.age.secrets.wifi.path;
    # networks.Aether.pskRaw = "e1b18af54036c5c9a747fe681c6a694636d60a5f8450f7dec0d76bc93e2ec85a";
    networks.Schilfpalast.pskRaw = "ext:schilfpalast";
  };
}
