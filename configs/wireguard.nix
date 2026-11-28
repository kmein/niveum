{
  config,
  pkgs,
  lib,
  ...
}:
let
  machines = import ../lib/machines.nix;
  hostname = config.networking.hostName;
  self = machines.${hostname};
  inherit (pkgs.lib.niveum.wireguard) port names;
  # laptops don't peer with each other: neither knows the other's endpoint
  peers = lib.filterAttrs (
    name: m: m ? wireguard && name != hostname && (self ? externalIp || m ? externalIp)
  ) machines;
in
# only machines with a wireguard entry in lib/machines.nix have a
# <hostname>-wireguard-privateKey.age secret
lib.mkIf (machines ? ${hostname} && machines.${hostname} ? wireguard) {
  age.secrets.wireguard = {
    file = ../secrets/${hostname}-wireguard-privateKey.age;
    mode = "400";
  };

  networking.wireguard.interfaces.niveum = {
    ips = [ "${self.wireguard.ip}/24" ];
    privateKeyFile = config.age.secrets.wireguard.path;
    listenPort = lib.mkIf (self ? externalIp) port;
    # no persistentKeepalive: servers never initiate towards laptops
    peers = lib.mapAttrsToList (
      name: m:
      {
        inherit name;
        inherit (m.wireguard) publicKey;
        allowedIPs = [ "${m.wireguard.ip}/32" ];
      }
      // lib.optionalAttrs (m ? externalIp) { endpoint = "${m.externalIp}:${toString port}"; }
    ) peers;
  };

  networking.firewall.allowedUDPPorts = lib.mkIf (self ? externalIp) [ port ];

  networking.hosts = lib.mapAttrs' (host: lib.nameValuePair machines.${host}.wireguard.ip) names;

  assertions = map (name: {
    assertion = lib.hasInfix pkgs.lib.niveum.wireguardOnly (
      config.services.nginx.virtualHosts.${name}.extraConfig or ""
    );
    message = "${name} is in lib.niveum.wireguard.names.${hostname}, but its vhost is not wireguardOnly";
  }) (names.${hostname} or [ ]);
}
