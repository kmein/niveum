{
  config,
  pkgs,
  lib,
  ...
}:
with lib; let
  netname = "retiolum";
  cfg = config.networking.retiolum;
in {
  options = {
    networking.retiolum.ipv4 = mkOption {
      type = types.str;
      description = ''
        own ipv4 address
      '';
    };
    networking.retiolum.ipv6 = mkOption {
      type = types.str;
      description = ''
        own ipv6 address
      '';
    };
    networking.retiolum.nodename = mkOption {
      type = types.str;
      default = config.networking.hostName;
      description = ''
        tinc network name
      '';
    };
  };

  config = {
    services.tinc.networks.${netname} = {
      name = cfg.nodename;
      hosts = builtins.mapAttrs
      (name: _: builtins.readFile "${<retiolum/hosts>}/${name}")
      (builtins.readDir <retiolum/hosts>);
      rsaPrivateKeyFile = toString <system-secrets/retiolum.key>;
      ed25519PrivateKeyFile = toString <system-secrets/retiolum.ed25519>;
      extraConfig = ''
        LocalDiscovery = yes
        AutoConnect = yes
      '';
    };

    networking.extraHosts = builtins.readFile (toString <retiolum/etc.hosts>);

    environment.systemPackages = [config.services.tinc.networks.${netname}.package];

    networking.firewall = {
      allowedTCPPorts = [655];
      allowedUDPPorts = [655];
    };
    #services.netdata.portcheck.checks.tinc.port = 655;

    systemd.network = {
      enable = true;
      networks = {
        "${netname}".extraConfig = ''
          [Match]
          Name = tinc.${netname}

          [Network]
          Address=${cfg.ipv4}/12
          Address=${cfg.ipv6}/16
        '';
      };
    };
  };
}
