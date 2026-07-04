{
  config,
  pkgs,
  lib,
  ...
}:
let
  machines = import ../lib/machines.nix;
  hostname = config.networking.hostName;
  hyprspacePeers = lib.mapAttrsToList (name: host: {
    id = host.hyprspace.id;
    inherit name;
  }) (lib.filterAttrs (name: host: host ? hyprspace && name != hostname) machines);
in
# only machines with a hyprspace entry in lib/machines.nix have a
# <hostname>-hyprspace-privateKey.age secret
lib.mkIf (machines ? ${hostname} && machines.${hostname} ? hyprspace) {
  age.secrets.hyprspace = {
    file = ../secrets/${config.networking.hostName}-hyprspace-privateKey.age;
    mode = "400";
  };

  services.hyprspace = {
    enable = true;
    privateKeyFile = config.age.secrets.hyprspace.path;
    settings = {
      peers = hyprspacePeers;
    };
  };

  networking.hosts = lib.mapAttrs' (name: address: {
    name = address.ipv6;
    value = [ "${name}.h" ];
  }) (lib.filterAttrs (name: address: address ? ipv6) pkgs.lib.niveum.hyprspaceAddresses);

  networking.firewall.allowedUDPPorts = [ 8001 ];
  networking.firewall.allowedTCPPorts = [ 8001 ];
}
