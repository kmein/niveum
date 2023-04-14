{
  config,
  lib,
  pkgs,
  ...
}: let
  specusMachines = {
    servers = {
      makanek = {
        ipv4 = "10.100.0.1";
        publicKey = "KhcScd4fBpdhQzK8Vc+1mEHQMQBpbKBUPB4oZ7skeSk=";
      };
      ful = {
        ipv4 = "10.100.0.2";
        publicKey = "0Y7+zoXkWJGVOWWnMjvYjtwP+WpggAlmkRbgMw0z8Dk=";
      };
    };
    clients = {
      kabsa = {
        ipv4 = "10.100.0.101";
        publicKey = "nRkzoRi9crKHF7263U37lt4GGL7/8637NBSKjifI9hY=";
      };
    };
  };
in {
  options.services.specus = {
    server = {
      enable = lib.mkEnableOption "Specus private VPN (server)";
    };
    client = {
      enable = lib.mkEnableOption "Specus private VPN (client)";
    };
    privateKeyFile = lib.mkOption {
      type = lib.types.path;
      description = "Private key file of the server/client machine";
    };
  };

  config = let
    cfg = config.services.specus;
    specusPort = 22;
  in
    {
      assertions = [
        {
          assertion =
            !(cfg.server.enable && cfg.client.enable);
          message = "specus: systems cannot be client and server at the same time";
        }
      ];
    }
    // lib.mkIf cfg.server.enable {
      networking.nat = {
        enable = true;
        externalInterface = "eth0"; # TODO
        internalInterfaces = ["specus"];
      };
      networking.firewall.allowedUDPPorts = [specusPort];
      networking.wireguard.interfaces.specus = {
        ips = ["${specusMachines.servers.${config.networking.hostName}.ipv4}/24"];
        # For this to work you have to set the dnsserver IP of your router (or dnsserver of choice) in your clients
        postSetup = ''
          ${pkgs.iptables}/bin/iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        postShutdown = ''
          ${pkgs.iptables}/bin/iptables -t nat -D POSTROUTING -s 10.100.0.0/24 -o eth0 -j MASQUERADE
        '';
        listenPort = specusPort;
        privateKeyFile = cfg.privateKeyFile;
        peers =
          lib.mapAttrsToList (clientName: clientConfig: {
            publicKey = clientConfig.publicKey;
            allowedIPs = ["${clientConfig.ipv4}/32"];
          })
          specusMachines.clients;
      };
    }
    // lib.mkIf cfg.client.enable {
      networking.firewall.allowedUDPPorts = [specusPort];
      networking.wireguard.interfaces = lib.attrsets.mapAttrs' (serverName: serverConfig:
        lib.nameValuePair "specus-${serverName}" {
          ips = ["${specusMachines.clients.${config.networking.hostName}.ipv4}/24"];
          listenPort = specusPort;
          privateKeyFile = cfg.privateKeyFile;
          peers = [
            {
              allowedIPs = ["0.0.0.0/0"];
              endpoint = "${(import ../lib/external-network.nix).${serverName}}:${toString specusPort}";
              persistentKeepalive = 25;
              publicKey = serverConfig.publicKey;
            }
          ];
        })
      specusMachines.servers;
    };
}
