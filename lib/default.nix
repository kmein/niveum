{ lib, pkgs }:
let
  systems = import ./systems.nix;
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

  sshPort = 22022;

  theme = {
    gtk = {
      name = "Adwaita-dark";
      package = pkgs.gnome-themes-extra;
    };
    icon = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    cursor = {
      name = "capitaine-cursors-white";
      package = pkgs.capitaine-cursors;
    };
  };

  defaultApplications = {
    terminal = "alacritty";
    browser = "${pkgs.firefox}/bin/firefox";
    fileManager = "${pkgs.pcmanfm}/bin/pcmanfm";
  };

  retiolumAddresses = lib.mapAttrs (_: v: { inherit (v.retiolum) ipv4 ipv6; }) (
    lib.filterAttrs (_: v: v ? "retiolum") systems
  );
  externalNetwork = lib.mapAttrs (_: v: v.externalIp) (
    lib.filterAttrs (_: v: v ? "externalIp") systems
  );
  localAddresses = lib.mapAttrs (_: v: v.internalIp) (
    lib.filterAttrs (_: v: v ? "internalIp") systems
  );
  myceliumAddresses = lib.mapAttrs (_: v: v.mycelium.ipv6) (
    lib.filterAttrs (_: v: v ? "mycelium") systems
  );
  syncthingIds = lib.mapAttrs (_: v: { id = v.syncthingId; }) (
    lib.filterAttrs (_: v: v ? "syncthingId") systems
  );

  email = import ./email.nix;

  systems = systems;

  kieran = {
    github = "kmein";
    email = "kmein@posteo.de";
    name = "Kierán Meinhardt";
    sshKeys = [
      systems.fatteh.sshKey
      systems.manakish.sshKey
      systems.kabsa.sshKey
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
