{ config, pkgs, lib, ... }:

with lib;

let
  netname = "retiolum";
  cfg = config.networking.retiolum;

  retiolum = pkgs.fetchFromGitHub {
    owner = "krebs";
    repo = netname;
    rev = "76e8de36d4ac06bcfaf551946aa2f6a41d9e6555";
    sha256 = "0yhmmvg6gm5yxs1gzszl19bs6y302yjg81a7sckfglwzcql3q0wf";
  };
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
      extraConfig = ''
        LocalDiscovery = yes
        AutoConnect = yes
      '';
    };
    systemd.services."tinc.${netname}" = {
      preStart = ''
        cp -R ${retiolum}/hosts /etc/tinc/retiolum/ || true
      '';
    };

    networking.extraHosts =
      builtins.readFile (toString "${retiolum}/etc.hosts");

    environment.systemPackages =
      [ config.services.tinc.networks.${netname}.package ];

    networking.firewall = {
      allowedTCPPorts = [ 655 ];
      allowedUDPPorts = [ 655 ];
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
