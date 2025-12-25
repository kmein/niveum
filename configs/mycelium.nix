{ lib, pkgs, ... }:
{
  services.mycelium = {
    enable = true;
    openFirewall = true;
  };

  networking.hosts = lib.mapAttrs' (name: address: {
    name = address;
    value = [ "${name}.m" ];
  }) pkgs.lib.niveum.myceliumAddresses;
}
