{ lib, pkgs }:
let
  machines = import ./machines.nix;

  # Generated on kabsa, but the private half was copied to fatteh and manakish
  # and outlives that host; it signs commits everywhere.
  signingKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDyTnGhFq0Q+vghNhrqNrAyY+CsN7nNz8bPfiwIwNpjk";
in
{
  tmpfilesConfig =
    {
      type,
      path,
      mode ? "-",
      user ? "-",
      group ? "-",
      age ? "-",
      argument ? "-",
    }:
    "${type} '${path}' ${mode} ${user} ${group} ${age} ${argument}";

  restic =
    let
      host = "zaatar.r";
      port = 3571;
    in
    {
      inherit host port;
      repository = "rest:http://${host}:${toString port}/";
    };

  domain = "kmein.de";

  # node_exporter's textfile collector reads *.prom from here, for metrics that
  # come from a timer rather than an exporter
  textfileDirectory = "/var/lib/prometheus-node-exporter";

  remoteDir = "/home/kfm/remote";

  firewall = {
    accept =
      {
        source,
        protocol,
        dport,
      }:
      "nixos-fw -s ${lib.escapeShellArg source} -p ${lib.escapeShellArg protocol} --dport ${lib.escapeShellArg (toString dport)} -j nixos-fw-accept";
    addRules = lib.concatMapStringsSep "\n" (rule: "iptables -A ${rule}");
    removeRules = lib.concatMapStringsSep "\n" (rule: "iptables -D ${rule} || true");
  };

  serveHtml = file: pkgs: ''
    default_type "text/html";
    root ${
      pkgs.linkFarm "www" [
        {
          name = "index.html";
          path = file;
        }
      ]
    };
    index index.html;
  '';

  # Sandbox for a systemd service that talks to the network and touches nothing
  # else: `serviceConfig = pkgs.lib.niveum.hardening // { ... }`, then loosen
  # what the service needs. Everything is mkDefault, so plain definitions win,
  # including those of the NixOS module the service comes from.
  # Check with `systemd-analyze security <unit>`; a denied syscall shows up in
  # the journal as EPERM, a read-only path as EROFS.
  hardening = lib.mapAttrs (_: lib.mkDefault) {
    CapabilityBoundingSet = "";
    NoNewPrivileges = true;
    ProtectSystem = "strict";
    ProtectHome = true;
    PrivateTmp = true;
    PrivateDevices = true;
    ProtectKernelTunables = true;
    ProtectKernelModules = true;
    ProtectKernelLogs = true;
    ProtectControlGroups = true;
    ProtectClock = true;
    ProtectHostname = true;
    ProtectProc = "invisible";
    RestrictNamespaces = true;
    RestrictRealtime = true;
    RestrictSUIDSGID = true;
    LockPersonality = true;
    MemoryDenyWriteExecute = true; # breaks JITs: node, java
    RestrictAddressFamilies = [
      "AF_UNIX"
      "AF_INET"
      "AF_INET6"
    ];
    SystemCallArchitectures = "native";
    SystemCallFilter = [
      "@system-service"
      "~@privileged"
      "~@resources"
    ];
    SystemCallErrorNumber = "EPERM";
    UMask = "0077";
  };

  sshPort = 22022;

  retiolumAddresses = lib.mapAttrs (_: v: { inherit (v.retiolum) ipv4 ipv6; }) (
    lib.filterAttrs (_: v: v ? "retiolum") machines
  );
  externalNetwork = lib.mapAttrs (_: v: v.externalIp) (
    lib.filterAttrs (_: v: v ? "externalIp") machines
  );
  localAddresses = lib.mapAttrs (_: v: v.internalIp) (
    lib.filterAttrs (_: v: v ? "internalIp") machines
  );
  hyprspaceAddresses = lib.mapAttrs (_: v: v.hyprspace) (
    lib.filterAttrs (_: v: v ? "hyprspace") machines
  );
  torAddresses = lib.mapAttrs (_: v: v.torAddress) (
    lib.filterAttrs (_: v: v ? "torAddress") machines
  );
  syncthingIds = lib.mapAttrs (_: v: { id = v.syncthingId; }) (
    lib.filterAttrs (_: v: v ? "syncthingId") machines
  );

  email =
    let
      thunderbirdProfile = "donnervogel";
    in
    {
      inherit thunderbirdProfile;
      defaults = {
        thunderbird = {
          enable = true;
          profiles = [ thunderbirdProfile ];
        };
        aerc.enable = true;
        realName = "Kierán Meinhardt";
        folders.inbox = "INBOX";
      };
    };

  machines = machines;

  kieran = {
    github = "kmein";
    email = "kmein@posteo.de";
    name = "Kierán Meinhardt";
    pronouns = builtins.concatStringsSep "/" [
      "er"
      "he"
      "is"
      "οὗτος"
      "هو"
      "ⲛ̄ⲧⲟϥ"
      "он"
      "han"
      "सः"
    ];
    inherit signingKey;
    sshKeys = [
      machines.fatteh.sshKey
      machines.manakish.sshKey
      signingKey
    ];
  };

  ignorePaths = [
    "*~"
    ".stack-work/"
    "__pycache__/"
    ".mypy_cache/"
    "*.py[co]"
    "*.o"
    "*.hi"
    "*.aux"
    "*.bbl"
    "*.bcf"
    "*.blg"
    "*.fdb_latexmk"
    "*.fls"
    "*.out"
    "*.run.xml"
    "*.toc"
    "*.bbl"
    "*.class"
    "*.dyn_hi"
    "*.dyn_o"
    "dist/"
    ".envrc"
    ".direnv/"
    "dist-newstyle/"
    ".history"
  ];
}
