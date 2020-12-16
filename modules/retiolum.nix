{ config, pkgs, lib, ... }:
with lib;
let
  stockholm-systems =
  let systemsDir = <stockholm> + "/krebs/1systems";
   in genAttrs
     (attrNames (filterAttrs (_: value: value == "directory") (builtins.readDir systemsDir)))
     (name: import <nixpkgs/nixos> {
       configuration = import (systemsDir + "/${name}/config.nix");
     });

  hostsPackage = stockholm-systems.filebitch.config.krebs.tinc.retiolum.hostsPackage;

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
      extraConfig = ''
        LocalDiscovery = yes
        AutoConnect = yes
      '';
    };

    # environment.etc."tinc/retiolum".source = hostsPackage;

    systemd.services."tinc.${netname}" = {
      preStart = ''
        set -eu

        mkdir -p /etc/tinc/${netname}/hosts/
        cp ${hostsPackage}/* /etc/tinc/${netname}/hosts/
      '';

      # Some hosts require VPN for nixos-rebuild, so we don't want to restart it on update
      reloadIfChanged = true;
      # also in https://github.com/NixOS/nixpkgs/pull/106715
      serviceConfig.ExecReload = "${config.services.tinc.networks.${netname}.package}/bin/tinc -n ${netname} reload";
    };

    networking.extraHosts =
      # TODO generate from stockholm
      builtins.readFile (toString <retiolum/etc.hosts>);

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
