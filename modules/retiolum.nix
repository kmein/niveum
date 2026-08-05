# kartei-ng is data-only, so the retiolum module lives here.
{
  config,
  lib,
  pkgs,
  self,
  ...
}:
let
  inherit (self.inputs) kartei-ng tincr;

  cfg = config.networking.retiolum;

  addrsOf =
    net: net.addrs or (lib.optional (net ? ip4) net.ip4.addr ++ lib.optional (net ? ip6) net.ip6.addr);

  hostsIn = netname: lib.filterAttrs (_: host: host.nets ? ${netname}) kartei-ng.hosts;

  retiolumHosts = lib.filterAttrs (_: host: host.nets.retiolum ? tinc) (hostsIn "retiolum");

  # tincr is SPTPS-only; RSA-only peers would just log a refusal per attempt.
  tincHosts = lib.mapAttrs (_: tincHostConfig) (
    lib.filterAttrs (_: host: host.nets.retiolum.tinc ? pubkey_ed25519) retiolumHosts
  );

  tincHostConfig =
    host:
    let
      inherit (host.nets.retiolum) tinc;
      # an `internet` net is kartei's `via`: dialable, and tinc weighs it itself
      addresses = lib.optionals (host.nets ? internet) (addrsOf host.nets.internet);
      weight = tinc.weight or (if host.nets ? internet then null else 300);
    in
    lib.concatStringsSep "\n" (
      map (addr: "Address = ${addr} ${toString (tinc.port or 655)}") addresses
      ++ map (subnet: "Subnet = ${subnet}") (tinc.subnets or [ ])
      ++ map (subnet: "Subnet = ${subnet}") (addrsOf host.nets.retiolum)
      ++ [
        (tinc.extraConfig or "")
        tinc.pubkey
      ]
      ++ lib.optional (tinc ? pubkey_ed25519) "Ed25519PublicKey = ${tinc.pubkey_ed25519}\n"
      ++ lib.optional (weight != null) "Weight = ${toString weight}"
    );

  netHostsLines =
    netname: tld: withV4: hosts:
    lib.concatStrings (
      lib.mapAttrsToList (
        name: host:
        let
          net = host.nets.${netname};
          aliases = lib.concatStringsSep " " (lib.unique ([ "${name}.${tld}" ] ++ net.aliases or [ ]));
        in
        lib.optionalString (withV4 && net ? ip4) "${net.ip4.addr} ${aliases}\n"
        + lib.optionalString (net ? ip6) "${net.ip6.addr} ${aliases}\n"
      ) hosts
    );

  hostsLines =
    withV4:
    netHostsLines "retiolum" "r" withV4 retiolumHosts
    + netHostsLines "internet" "i" withV4 (hostsIn "internet");

  own = kartei-ng.hosts.${cfg.nodename}.nets.retiolum;
in
{
  imports = [ tincr.nixosModules.tincr ];

  options.networking.retiolum = {
    nodename = lib.mkOption {
      type = lib.types.str;
      default = config.networking.hostName;
      defaultText = lib.literalExpression "config.networking.hostName";
      description = "tinc node name of this machine inside retiolum.";
    };
    ipv4 = lib.mkOption {
      type = lib.types.nullOr lib.types.str;
      default = own.ip4.addr or null;
      defaultText = "looked up in kartei by nodename";
      description = "Own retiolum IPv4 address.";
    };
    ipv6 = lib.mkOption {
      type = lib.types.str;
      default = own.ip6.addr;
      defaultText = "looked up in kartei by nodename";
      description = "Own retiolum IPv6 address.";
    };
    port = lib.mkOption {
      type = lib.types.port;
      default = 655;
      description = "TCP/UDP port tincd listens on.";
    };
    ed25519PrivateKeyFile = lib.mkOption {
      type = lib.types.path;
      description = "Path to this node's Ed25519 private key.";
    };
    extraHosts = lib.mkOption {
      type = lib.types.bool;
      default = true;
      description = ''
        Whether to add all retiolum peers to /etc/hosts, so ‹node.r› resolves
        before tincr's DNS stub is up (e.g. nixos-rebuild over the mesh).
      '';
    };
  };

  config = {
    services.tincr = {
      package = lib.mkDefault tincr.packages.${pkgs.stdenv.hostPlatform.system}.tincd;
      networks.retiolum = {
        nodeName = cfg.nodename;
        listenPort = cfg.port;
        openFirewall = true;
        inherit (cfg) ed25519PrivateKeyFile;
        hosts = tincHosts;
        connectTo = [
          "eve"
          "eva"
          "ni"
          "prism"
        ];
        # retiolum incident 2d2ab95f0: broadcast loops caused a mesh-wide storm
        extraConfig = ''
          LocalDiscovery = yes
          Broadcast = no
        '';
        addresses = lib.optional (cfg.ipv4 != null) "${cfg.ipv4}/12" ++ [ "${cfg.ipv6}/16" ];
        interfaceName = "tinc.retiolum";
        dns = {
          enable = true;
          suffix = "r";
          address4 = "10.243.0.53";
          address6 = "42::53";
        };
      };
    };

    # measured with `ping -6 -s 1378`; pinned against PMTU blackholes
    systemd.network.networks."40-tincr-retiolum".linkConfig.MTUBytes = "1377";

    networking.extraHosts = lib.mkIf cfg.extraHosts (hostsLines (cfg.ipv4 != null));

    environment.systemPackages = [
      config.services.tincr.networks.retiolum.package
    ];

    # setup-etc won't replace the real directories the old services.tinc left
    # behind; "+" runs as root because /etc/tinc is root-owned and tincd is not
    systemd.services.tincr-retiolum.serviceConfig.ExecStartPre = lib.mkBefore [
      "+${pkgs.writeShellScript "tincr-retiolum-migrate" ''
        for name in hosts invitations; do
          d=/etc/tinc/retiolum/$name
          if [ -d "$d" ] && [ ! -L "$d" ]; then
            rm -rf "$d"
          fi
        done
        ln -sfn ${config.environment.etc."tinc/retiolum/hosts".source} /etc/tinc/retiolum/hosts
      ''}"
    ];
  };
}
