{ lib, ... }:
let
  myceliumAddresses = import ../lib/mycelium-network.nix;
in
{
  services.mycelium = {
    enable = true;
    openFirewall = true;
  };

  networking.hosts = lib.mapAttrs' (name: address: {
    name = address;
    value = [ "${name}.m" ];
  }) myceliumAddresses;
}
