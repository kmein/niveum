{ config, pkgs, lib, ... }:

with lib;

let

  netname = "retiolum";
  cfg = config.networking.retiolum;

  retiolum = pkgs.fetchFromGitHub {
    owner = "krebs";
    repo = netname;
    rev = "51fad505e3f6ad0cc82da46f05fe650d27766a14";
    sha256 = "1v4r1rf32jdx6y3phqmsxb1264ywyly3h53p3iyj0rl4izvdpd5h";
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

    networking.extraHosts = builtins.readFile (toString "${retiolum}/etc.hosts");

    environment.systemPackages = [ config.services.tinc.networks.${netname}.package ];

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
